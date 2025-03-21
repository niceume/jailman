(import (gauche parseopt))

(define (jailman-remove args)
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
      (when (eqv?
             (cdr (jailman-get-status
                   name
                   (running-jails-alist 'include-dying)))
             'running)
        (display (string-append "ERROR: running jail cannot be removed: " name "\n"))
        (exit 1))
      (let ((config-path (jailman-get-config-path name))
          (container-path (jailman-get-container-path name)))
        (if (file-exists? config-path)
            (begin
              (when
                  (ask-Yn (string-append
                           "Do you really delete config for " name))
                (when (delete-file config-path)
                  (display "Deleted") (newline)
                  )))
            (begin
              (display "config file does not exit")
              (newline)))
        (if (file-exists? container-path)
            (begin
              (when
                  (ask-Yn (string-append
                           "Do you really delete container for " name))
                (sys-system
                 #"chflags -R 0 ~container-path")
                (when (do-process `(rm -R -f ,container-path))
                  (display "Deleted") (newline))))
            (begin
              (display "container does not exit")
              (newline)))))))
