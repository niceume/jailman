(import (gauche parseopt) (gauche process)
        (scheme regex) (srfi 152))

(define (jailman-start args)
  (let1 unknown-opts '()
    (let-args args
      ((name "name=s")
         (else (option rest cont)
               (set! unknown-opts
                     (append unknown-opts
                             (list option)))
               (cont rest)
               )
         . rest-args)
        (exit-when-unknown-opts-or-rest-args-exist
         unknown-opts rest-args)
        (unless name
          (display "ERROR: name needs to be specified\n")
          (exit 1))
        (let*
          ((config-path
            (jailman-get-config-path name))
           (container-path
            (jailman-get-container-path name))
           (params
            (call-with-input-file config-path
              (lambda (in)
                (read in))))
           (jail-name
            (string-append "name=" name))
           (jail-hostname
            (string-append "host.hostname="
                           (alist-ref params 'hostname)))
           (jail-path
            (string-append "path="
                           container-path))
         (jail-allow-raw-sockets "allow.raw_sockets")
         (jail-exec-clean "exec.clean")
         (jail-mount-devfs "mount.devfs")
         (devfs-ruleset (alist-ref params 'devfs-ruleset))
         (jail-devfs-ruleset
          (if devfs-ruleset
              #"devfs_ruleset=~devfs-ruleset"
              #f))
         (vnet (alist-ref params 'vnet)))
          (if (not vnet)
              (begin
                (let*
                    ((jail-exec-start
                      (string-append
                       "exec.start="
                       "/bin/sh /etc/rc; "))
                     (interface-addr-ip4
                      (alist-ref params 'interface-addr-ip4))
                     (jail-ip4
                      (if interface-addr-ip4
                          "ip4=new"
                          "ip4=inherit" ))
                     (jail-ip4-addr
                      (if interface-addr-ip4
                          #"ip4.addr=~interface-addr-ip4"
                          #f )))
                  (string-list->do-process
                   (list "jail" "-cm"
                         jail-name
                         jail-hostname
                         jail-path
                         jail-exec-start
                         jail-allow-raw-sockets
                         jail-exec-clean
                         jail-mount-devfs
                         jail-devfs-ruleset
                         jail-ip4
                         jail-ip4-addr))
                  (let ((running-info-alist
                         (names->alist vnet interface-addr-ip4)))
                    (generate-jailman-running-info name running-info-alist))
                  ))
              (begin
            (let*
              ((bridge (alist-ref params 'use-bridge))
               (bridge-id
                (if bridge
                    (or (car (regexp-extract #/\d+/ bridge))
                        (error "ERROR: bridge interface without number specified"))
                    #f ))
               (jail-use-bridge
                (if bridge
                    (string-append
                     "interface=" bridge)
                    #f))
               (epaira
                (process-output->string
                 '(/sbin/ifconfig epair create)
                 ))
               (epair-num
                (car (regexp-extract #/\d+/ epaira)))
               (epairb (string-append "epair" epair-num "b"))
               (jail-vnet "vnet")
               (jail-vnet-interface
                (string-append "vnet.interface="
                               epairb))
               (vnet-epaira-ip4
                (alist-ref params 'vnet-epaira-ip4))
               (vnet-epairb-ip4
                (alist-ref params 'vnet-epairb-ip4))
               (vnet-gateway-ip4
                (if (not vnet-epaira-ip4)
                    (let ((matched
                           (regexp-search-line
                            #/^\s*inet\s+(\d+\.\d+\.\d+\.\d+)/
                            (process-output->string-list
                             `(/sbin/ifconfig ,(string->symbol bridge))))))
                      (if matched
                          (regexp-match-submatch matched 1)
                          (begin
                            (display "bridge interface is not assigned an IP address\n")
                            (display "try to use default route on host for jail\n")
                            (let ((matched-default-route
                                   (regexp-search-line
                                    #/^default\s+(\d+\.\d+\.\d+\.\d+)/
                                    (process-output->string-list
                                     `(netstat -rn)))))
                              (if matched-default-route
                                  (regexp-match-submatch
                                   matched-default-route 1)
                                  (error "default route on host cannot be found."))))))
                    (car
                     (regexp-extract #/\d+\.\d+\.\d+\.\d+/ vnet-epaira-ip4))))
               (jail-exec-prestart
                (string-append
                 "exec.prestart="
                 (string-join-without-false
                  (list
                   #"/sbin/ifconfig ~epaira up"
                   (if bridge
                       #"/sbin/ifconfig ~bridge addm ~epaira up"
                       #f )
                   (if vnet-epaira-ip4
                       #"/sbin/ifconfig ~epaira ~vnet-epaira-ip4 up"
                       #f )
                   )
                  " && ")
                 ";"))
               (jail-exec-start
                (string-append
                 "exec.start="
                 "/bin/sh /etc/rc && "
                 #"/sbin/ifconfig ~epairb ~vnet-epairb-ip4 up && "
                 #"/sbin/route add default ~vnet-gateway-ip4; "
                 )))
              (if
               (string-list->do-process
                (list "jail" "-cm"
                      jail-name
                      jail-hostname
                      jail-path
                      jail-use-bridge
                      jail-exec-prestart
                      jail-exec-start
                      jail-allow-raw-sockets
                      jail-exec-clean
                      jail-mount-devfs
                      jail-devfs-ruleset
                      jail-vnet
                      jail-vnet-interface))
               (begin
                 (let ((running-info-alist
                        (names->alist bridge vnet epaira)))
                   (generate-jailman-running-info
                    name running-info-alist)))
               (begin
                 (do-process
                  `(/sbin/ifconfig ,bridge deletem ,epaira))
                 (do-process
                  `(/sbin/ifconfig ,epaira destroy))
                 )))))))))

(define (generate-jailman-running-info name running-info-alist)
  (let ((running-info-path (jailman-get-running-info-path name)))
    (call-with-output-file running-info-path
      (lambda (out)
        (display ";; -*- mode: scheme -*-\n" out)
        (write running-info-alist out)))))

(define (regexp-search-line regexp lines)
  (unless (is-a? lines <list>)
    (error "lines need to be a list"))
  (cond
   ((null? lines) #f)
   (else
    (let ((matched
           (regexp-search regexp (car lines))))
      (if matched
          matched
          (regexp-search-line regexp (cdr lines)))))))

(define (string-join-without-false lst delim)
  (string-join
   (filter (lambda (x) x) lst)
   delim))
