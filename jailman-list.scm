(import (scheme base)
        (scheme read)
        (scheme sort)
        (gauche base)
        (file util))

(define (display-jail-params params)
  (display "(")
  (display (alist-ref params 'hostname)) (display " ")
  (if (not (alist-ref params 'vnet))
      (begin
        (display (alist-ref params 'interface-addr-ip4)))
      (begin
        (display (alist-ref params 'bridge-interface)) (display " ")
        (display (alist-ref params 'vnet-epaira-ip4)) (display " ")
        (display (alist-ref params 'vnet-epairb-ip4))))
  (display ")"))

(define (jailman-list args)
  (let1 unknown-opts '()
    (let-args args
      ((only-names "only-names")
       (else (option rest cont)
             (set! unknown-opts
                   (append unknown-opts
                           (list option)))
             (cont rest)
             )
       . rest-args)
      (exit-when-unknown-opts-or-rest-args-exist
       unknown-opts rest-args)
      (let ((running-jails-alist
             (running-jails-alist)))
        (let loop
            ((jailman-names
              (list-sort string<?
                         (lset-union string=?
                                     (jailman-get-list-containers)
                                     (jailman-get-list-configs)))))
          (unless (null? jailman-names)
            (let* 
                ((jail-name (car jailman-names))
                 (status
                  (jailman-get-status jail-name running-jails-alist))
                 (container-status
                  (car status))
                 (container-config-status
                  (cdr status)))
              (display jail-name)
              (unless only-names
                (display #\tab)
                (display container-status)
                (display #\tab)
                (case container-config-status
                  ((running container-config-exist)
                   (display-jail-params
                    (jailman-get-config-params jail-name)))
                  ((non-config)
                   (display "no-config"))
                  ((non-container)
                   (display "no-container"))
                  (else =>
                        (lambda (x)
                          (display
                           (string-append
                            "unknown-status:"
                            (symbol->string x)))))))
              (newline)
              (loop (cdr jailman-names)))))))))
