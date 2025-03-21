(import (scheme list)
        (srfi 26))

(define (jailman-list-media args)
  (let1 unknown-opts '()
    (let-args args
      ((else (option rest cont)
             (set! unknown-opts
                   (append unknown-opts
                           (list option)))
             (cont rest)
             )
       . rest-args)
      (exit-when-unknown-opts-or-rest-args-exist
       unknown-opts rest-args)
      (let ((type 'base))
        (case type
          ((base)
           (let ((media-list
                  (jailman-list-media-of type)))
             (unless (null? media-list)
               (display "[base]") (newline)
               (let ((media-alist
                      (zip
                       (map (cut jailman-extract-media-version <> type)
                            media-list)
                       media-list)))
                 (display-alist media-alist)))))
          (else
           (display "ERROR: unknown type is specified")
           (exit 1)))))))
