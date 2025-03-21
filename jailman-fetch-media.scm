(import (gauche parseopt)
        (scheme process-context))

(define (jailman-fetch-media args)
  (let1 unknown-opts '()
    (let-args args
      ((version "version=s")
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
      (let* ((arch (get-current-arch))
             (type 'base)
             (media-url
              (or (jailman-get-media-url type arch version)
                  (begin (display "media url is undecided") (exit 1))))
             (file-path (jailman-get-media-path type version)))
        (if (jailman-media-exist? type version)
            (begin
             (display
              (string-append
               "Media already exists:"
               file-path))
             (newline)
             (exit 1))
            (begin
              (create-directory*
               (simplify-path
                (string-append file-path "/..")))
              (do-fetch media-url file-path)
              (newline)))))))
