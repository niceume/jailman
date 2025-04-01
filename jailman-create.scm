(import (gauche parseopt)
        (scheme regex)
        (scheme base)
        (gauche base))

(define-syntax confirm-option-defined
  (syntax-rules ()
    [(_ var ...)
     (begin
       (unless var
         (begin
           (display
            (string-append
             "ERROR: "
             (symbol->string 'var)
             " needs to be defined by "
             (alist-ref *option-alist* 'var)
             " option" ))
           (newline)
           (exit 1)
           )) ...) ]))

(define-syntax names->alist
  (syntax-rules ()
    [(_ var ...)
     `((var . ,var) ... )
     ]))

(define *option-alist*
  '((name . "name=s")
    (type . "type=y")
    (version . "version=s")
    (hostname . "hostname=s")
    (devfs-ruleset . "devfs-ruleset=s")
    (interface-addr-ip4 . "interface-addr-ip4=s")
    (vnet . "vnet")
    (use-bridge . "use-bridge=s")
    (vnet-epaira-ip4 . "vnet-epaira-ip4=s")
    (vnet-epairb-ip4 . "vnet-epairb-ip4=s")))

(define (jailman-create args)
  (let1 unknown-opts '()
        (let-args args
          ((name (alist-ref *option-alist* 'name))
           (type (alist-ref *option-alist* 'type) 'base)
           (version (alist-ref *option-alist* 'version))
           (hostname (alist-ref *option-alist* 'hostname))
           (devfs-ruleset (alist-ref *option-alist* 'devfs-ruleset))
           (interface-addr-ip4 (alist-ref *option-alist* 'interface-addr-ip4))
           (vnet (alist-ref *option-alist* 'vnet))
           (use-bridge (alist-ref *option-alist* 'use-bridge))
           (vnet-epaira-ip4 (alist-ref *option-alist* 'vnet-epaira-ip4))
           (vnet-epairb-ip4 (alist-ref *option-alist* 'vnet-epairb-ip4))
           (else (option rest cont)
                 (set! unknown-opts
                       (append unknown-opts
                               (list option)))
                 (cont rest)
                 )
           . rest-args)
          (exit-when-unknown-opts-or-rest-args-exist
           unknown-opts rest-args)
          (begin
            (confirm-option-defined
             name type version hostname)
            (validate-devfs-ruleset devfs-ruleset)
            (if (not vnet)
                (begin
                  (when interface-addr-ip4
                    (validate-interface-addr-ip4 interface-addr-ip4)))
                (begin
                  (when use-bridge
                    (validate-bridge-interface use-bridge))
                  (validate-vnet-options
                   vnet vnet-epaira-ip4 vnet-epairb-ip4)))
            (confirm-config-does-not-exist name)
            (confirm-container-does-not-exist name)
            (confirm-media-exists type version))

          ;; Expand base file
          (display "Container is being created")(newline)
          (do-process
           `(mkdir ,(jailman-get-container-path name)))

          (do-process
           `(tar -xf ,(jailman-get-media-path type version)
                 -C ,(jailman-get-container-path name)
                 --unlink))

          (let ((resolv-path
                 (string-append
                  (jailman-get-container-path name)
                  "/etc/resolv.conf"))
                (localtime-path
                 (string-append
                  (jailman-get-container-path name)
                  "/etc/localtime")))
            (do-process
             `(cp /etc/resolv.conf ,resolv-path))
            (do-process
             `(cp /etc/localtime ,localtime-path)))

          ;; Generate config file
          (display "Config is being created")(newline)
          (let ((setting-alist
                 (names->alist hostname devfs-ruleset interface-addr-ip4
                               vnet use-bridge
                               vnet-epaira-ip4 vnet-epairb-ip4)))
            (generate-jailman-setting name setting-alist))
          (display "Finished")(newline)
          )))

(define (extract-numbers-from-devfs-rules lines)
  (cond ((null? lines) '())
        (else
         (if (not (regexp-search #/^\s*#/ (car lines)))
           (let ((matched (regexp-search #/\[\w+=(\d+)\]/ (car lines))))
             (if matched
                 (begin
                   (cons
                    (regexp-match-submatch matched 1)
                    (extract-numbers-from-devfs-rules (cdr lines))))
                 (extract-numbers-from-devfs-rules (cdr lines))))
           (extract-numbers-from-devfs-rules (cdr lines))))))

(define (call-with-file-content-lines path proc)
  (call-with-input-file
   path
   (lambda (port)
     (let ((lines (port->string-list port)))
       (proc lines)))))

(define (validate-devfs-ruleset ruleset-num)
  (when ruleset-num
    (let*
        ((ruleset-nums-custom
          (if (file-exists? "/etc/devfs.rules")
              (call-with-file-content-lines
               "/etc/devfs.rules"
               extract-numbers-from-devfs-rules)
              '()))
         (ruleset-nums-defaults
          (if (file-exists? "/etc/defaults/devfs.rules")
              (call-with-file-content-lines
               "/etc/defaults/devfs.rules"
               extract-numbers-from-devfs-rules)
              '()))
         (ruleset-nums
          (append ruleset-nums-custom ruleset-nums-defaults)))
      (if (member ruleset-num ruleset-nums)
          #t
          (begin
            (display "ERROR: number specified as devfs-ruleset cannot be found in devfs.rules\n")
            (display "       check /etc/devfs.rules and /etc/defaults/devfs.rules\n")
            (exit 1))))))

(define (validate-bridge-interface interface)
  (unless (regexp-matches? #/^bridge\d+/ interface)
    (display "ERROR: inteface not bridge is specified\n")
    (exit 1)))

(define (validate-interface-addr-ip4 interface-addr-ip4)
  (unless
      (or
       (regexp-matches?
        #/^\w+\|\d+\.\d+\.\d+\.\d+$/ ;; just interface|address
        interface-addr-ip4)
       (regexp-matches?
        #/^\w+\|\d+\.\d+\.\d+\.\d+\/\d+$/ ;; with /CIDR
        interface-addr-ip4)
       (regexp-matches?
        #/^\w+\|\d+\.\d+\.\d+\.\d+\/\d+\.\d+\.\d+\.\d+$/ ;; with /dotted decimal
        interface-addr-ip4))
    (display "ERROR: Interface with IPv4 address (+mask) needs to be specified\n")
    (exit 1)))

(define (validate-vnet-ip4 vnet-ip4)
  (unless
      (or (regexp-matches? #/^\d+\.\d+\.\d+\.\d+\/\d+$/ vnet-ip4)
          (regexp-matches? #/^\d+\.\d+\.\d+\.\d+\/\d+\.\d+\.\d+\.\d+$/ vnet-ip4))
    (display "ERROR: IPv4 address with subnet mask needs to be specified\n")
    (exit 1)))

(define (validate-vnet-options vnet vnet-epaira-ip4 vnet-epairb-ip4)
  (when vnet
    (unless vnet-epairb-ip4
      (display "ip4 address/subnet must be specified for epairb\n")
      (exit 1))
    (when vnet-epaira-ip4
      (validate-vnet-ip4 vnet-epaira-ip4))
    (when vnet-epairb-ip4
      (validate-vnet-ip4 vnet-epairb-ip4))))

(define (confirm-config-does-not-exist name)
  (when (member name (jailman-get-list-configs))
    (display "ERROR: The specified name already exists in configs\n")
    (exit 1)))

(define (confirm-container-does-not-exist name)
  (when (member name (jailman-get-list-containers))
    (display "ERROR: The specified name already exists in containers\n")
    (exit 1)))

(define (confirm-media-exists type version)
  (unless (jailman-media-exist? type version)
    (begin
      (display
       (string-append "ERROR: Specified media does not exist. "
                      "Please check version or type again.\n"))
      (exit 1))))

(define (confirm-interface-exists interface)
  (unless (member interface (list-network-interfaces))
    (begin
      (display "ERROR: Specified network interface does not exist\n")
      (exit 1))))

(define (generate-jailman-setting name setting-alist)
  (let ((config-path (jailman-get-config-path name)))
    (call-with-output-file config-path
      (lambda (out)
        (display ";; -*- mode: scheme -*-\n" out)
        (write setting-alist out)))))
