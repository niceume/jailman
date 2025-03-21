(import (gauche parseopt))

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
              ((bridge (alist-ref params 'bridge-interface))
               (bridge-id
                (if bridge
                    (car (regexp-extract #/\d+/ bridge))
                    (error "ERROR: bridge interface without number specified")))
               (jail-bridge-interface
                (string-append
                 "interface=" bridge))
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
                (car
                 (regexp-extract #/\d+\.\d+\.\d+\.\d+/ vnet-epaira-ip4)))
               (jail-exec-prestart
                (string-append
                 "exec.prestart="
                 #"/sbin/ifconfig ~epaira up && "
                 (when bridge
                   #"/sbin/ifconfig ~bridge addm ~epaira up && " )
                 #"/sbin/ifconfig ~epaira ~vnet-epaira-ip4 up;"
                 ))
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
                      jail-bridge-interface
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
