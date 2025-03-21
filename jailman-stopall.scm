(import (gauche parseopt)
        (file util))

(define (jailman-stopall args)
  (let1 unknown-opts '()
    (let-args args
      ((yes "yes")
       (else (option rest cont)
             (set! unknown-opts
                   (append unknown-opts
                               (list option)))
             (cont rest)
             )
       . rest-args)
      (exit-when-unknown-opts-or-rest-args-exist
       unknown-opts rest-args)
      (unless yes
        (unless
            (ask-Yn "Stop all the jails run by jailman: ")
          (exit 1)))
      (let loop ((running-jails-alist
                  (running-jails-alist)))
        (unless (null? running-jails-alist)
          (let*
              ((jail-name (caar running-jails-alist))
               (jail-path (cdar running-jails-alist)))
            (if (string=?
                 (simplify-path jail-path)
                 (simplify-path
                  (jailman-get-container-path jail-name)))
                (begin
                  (display
                   (string-append "[" jail-name "]\n"))
                  (jailman-stop `("--name" ,jail-name)))
                (display
                 (string-append
                jail-name
                ": this jail is not run by jailman\n"))))
          (loop (cdr running-jails-alist)))))))
