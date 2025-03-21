(import (gauche parseopt)
        (scheme process-context))

(define (jailman-remove-media args)
  (let1 unknown-opts '()
    (let-args args
      ((version "version=s")
       (type "type=s" 'base)
       (else (option rest cont)
             (set! unknown-opts
                   (append unknown-opts
                           (list option)))
             (cont rest)
             )
       . rest-args)
      (exit-when-unknown-opts-or-rest-args-exist
       unknown-opts rest-args)
      (unless version
        (display "ERROR: version needs to be specified\n")
        (exit 1)
        )
      (let ((file-path (jailman-get-media-path type version)))
        (unless (file-exists? file-path)
          (display
           (string-append
            "Media does not exist:"
            file-path))
          `   (exit 1))
        (delete-file file-path)))))
