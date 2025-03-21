(import (file util)
        (scheme list)
        (scheme file)
        (scheme regex)
        (gauche base)
        (gauche keyword)
        (gauche process))

(define (display-list list)
  (let loop ((lst list))
    (cond ((null? lst)  )
          (else
           (display (car lst))
           (newline)
           (loop (cdr lst))))))

(define (display-alist alist)
  (let loop ((alst alist))
    (cond ((null? alst)  )
          (else
           (display (caar alst))
           (display "\t")
           (display (cdar alist))
           (newline)
           (loop (cdr alst))))))

(define (ask-Yn message)
  (let loop ()
    (display message)
    (display " [Y/n]: ")
    (let ((reply (read-line)))
      (if (string=? reply "Y")
          #t
          (if (string=? reply "n")
              #f
              (begin
                (display "Please input Y or n")
                (newline)
                (loop)))))))

(define-syntax exit-when-unknown-opts-or-rest-args-exist
  (syntax-rules ()
    ((_ unknown-opts rest-args)
     (begin
       (unless (null? unknown-opts)
         (display "Unknown options are detected: ")
         (let loop ((unknowns unknown-opts))
           (cond ((null? unknowns) (newline))
                 (else (display (car unknowns)) (display " ")
                       (loop (cdr unknowns)))))
         (exit 1))
       (unless (null? rest-args)
         (display "Argument parsing stopped and ")
         (display "non option argument detected: ")
         (display (car rest-args)) (newline)
         (exit 1))))))

(define (jailman-get-media-url type arch version)
  (display arch)
  (display version)
  (case type
    ((base)
     (string-append
      "https://download.freebsd.org/ftp/releases/"
      arch
      "/"
      version
      "/base.txz"))
    (else
     (display "ERROR: unknown type is specified")
     #f)))

(define (jailman-list-media-of type)
  (case type
    ((base)
     (filter
      (lambda (file)
       regexp-matches?
       #/base\.txz$/
       file)
      (directory-list
       *jailman-media-dir*
       :children? #t)))
    (else
     (display "ERROR: unknown type is specified")
     #f)))

(define (jailman-get-media-path type version)
  (case type
    ((base)
     (string-append
      *jailman-media-dir*
      version
      "-base.txz"))
    (else
     (display "ERROR: unknown type is specified")
     #f)))

(define (jailman-media-exist? type version)
  (file-exists? (jailman-get-media-path type version)))

(define (jailman-extract-media-version str type)
  (case type
    ((base)
     (let
         ((matched (regexp-matches
                    #/([.\-a-zA-Z0-9]+)-base\.txz$/
                    str)))
       (if matched
           (regexp-match-submatch matched 1)
           #f )))
    (else
     (display "ERROR: unknown type is specified")
     #f )))

(define (jailman-get-list-configs)
  (directory-list
   *jailman-config-dir*
   :children? #t))

(define (jailman-get-config-path name)
  (string-append
   *jailman-config-dir*
   name ))

(define (jailman-get-config-params name)
  (let
    ((param-string
      (fold string-append ""
            (call-with-input-file (jailman-get-config-path name)
              (lambda (in)
                (filter
                 (complement (cut regexp-search #/^;/ <>))
                 (port->string-list in)))))))
    (read (open-input-string param-string))))

(define (jailman-get-list-containers)
  (directory-list
   *jailman-container-dir*
   :children? #t))

(define (jailman-get-container-path name)
  (string-append
   *jailman-container-dir*
   name
   ))

(define (jailman-get-running-info-path name)
  (string-append
   *jailman-running-info-dir*
   name
   ))

(define (jailman-check-permission place type)
  (let ((path
         (case place
           ((media) *jailman-media-dir*)
           ((config) *jailman-config-dir*)
           ((container) *jailman-container-dir*)
           ((running-info) *jailman-running-info-dir*)
           (else (display "unknown place specified for permission check\n")
                 (exit 1)))))
    (case type
      ((readable)
       (unless (file-is-readable? path)
         (display
          (string-append "ERROR: " path "is not readable\n"))
         (exit 1)))
      ((writable)
       (unless (file-is-writable? path)
         (display
          (string-append "ERROR: " path "is not writable\n"))
         (exit 1)))
      ((executable)
       (unless (file-is-executable? path)
         (display
          (string-append "ERROR: " path "is not executable\n"))
         (exit 1)))
      (else
       (display "unkonwn type specified for permission check\n")
       (exit 1)))))

(define (running-jails-alist . include-dying)
  (let* ((jls-output-list
          (if (null? include-dying)
              (process-output->string-list
               '(jls))
              (process-output->string-list
               '(jls -d)))))
    (if (<= (length jls-output-list) 1)
        '()
        (map
         (lambda (line)
           (let* ((elems (regexp-extract #/[\w\/]+/ line))
                  (jail-name (second
                              (reverse elems)))
                  (jail-path (first
                              (reverse elems))))
             `(,jail-name . ,jail-path)
             ))
         (cdr jls-output-list)))))

(define (jailman-get-status name running-jails-alist)
  (if (and (assoc name running-jails-alist)
           (string=?
            (simplify-path
             (cdr (assoc name running-jails-alist)))
            (simplify-path
             (jailman-get-container-path name))))
      '("running" . running)
      (begin
        (if (and
             (file-exists? (jailman-get-container-path name))
             (file-exists? (jailman-get-config-path name)))
            '("non-active" . container-config-exist)
            (if (file-exists? (jailman-get-config-path name))
                '("inconsistent" . non-container)
                '("inconsistent" . non-config))))))
