(import (gauche parseopt))

(define (jailman-login args)
  (let1 unknown-opts '()
    (let-args args
      ((name "name=s")
       (user "user=s")
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
      (if (not user)
          (string-list->do-process
           (list
            "jexec" name "login" "-f" "root"))
          (string-list->do-process
           (list
            "jexec" name "login" "-f" user))))))
