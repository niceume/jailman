(import (gauche parseopt)
        (scheme regex))

(define (jailman-stop args)
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
      (unless (eqv?
               (cdr (jailman-get-status
                     name
                     (running-jails-alist)))
               'running)
        (display (string-append "ERROR: jail is not running: " name "\n"))
        (exit 1))
      (let
        ((running-info-path
          (jailman-get-running-info-path name)))
        (unless (file-exists? running-info-path)
          (display (string-append "ERROR: running info is not found:" name "\n"))
          (exit 1))
        (let*
            ((params (call-with-input-file running-info-path
                       (lambda (in)
                         (read in))))
             (vnet (alist-ref params 'vnet))
             (jail-name
              (string-append "name=" name)))
          (if (not vnet)
              (begin
                (let*
                  ((interface-addr-ip4
                    (alist-ref params 'interface-addr-ip4)))
                  (if (not (do-process
                            `(jexec ,name /bin/sh /etc/rc.shutdown)))
                      (begin
                        (display "jail shutdown failed\n")
                        (exit 1))
                      (if (not (do-process
                                `(jail -rR ,name)))
                          (begin
                            (display "stopping jail failed\n")
                            (exit 1))
                          (begin
                            (when interface-addr-ip4
                              (let*
                                  ((interface-addr-ip4-matched
                                    (regexp-matches #/^(\w+)\|([0-9\.\/]+)$/
                                                    interface-addr-ip4))
                                   (interface
                                    (regexp-match-submatch
                                     interface-addr-ip4-matched 1))
                                   (addr-ip4
                                    (regexp-match-submatch
                                     interface-addr-ip4-matched 2)))
                                (do-process
                                 `(/sbin/ifconfig ,interface
                                                  inet
                                                  ,addr-ip4
                                                  delete))))
                            (do-process
                             `(/sbin/umount
                               ,(string-append (jailman-get-container-path name)
                                               "/dev")))
                            (display "successfully stopeed\n")
                            (delete-file running-info-path))))))
              (begin
                (let*
                    ((bridge
                      (alist-ref params 'bridge))
                     (epaira
                      (alist-ref params 'epaira)))
                  (if (not (do-process
                            `(jexec ,name /bin/sh /etc/rc.shutdown)))
                      (begin
                        (display "jail shutdown failed\n")
                        (exit 1))
                      (begin
                        (if (not (do-process
                                  `(jail -R -v ,name)))
                            (begin
                              (display "stopping jail failed\n")
                              (exit 1))
                            (begin
                              (when bridge
                                (do-process
                                 `(/sbin/ifconfig ,bridge deletem ,epaira)))
                              (do-process
                               `(/sbin/ifconfig ,epaira destroy))
                              (do-process
                               `(/sbin/umount
                                 ,(string-append (jailman-get-container-path name)
                                                 "/dev")))
                              (display "successfully stopeed\n")
                            (delete-file running-info-path)))))))))))))
