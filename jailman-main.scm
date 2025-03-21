(import (scheme base)
        (scheme write)
        (scheme process-context)
        (file util)
        (srfi 98))

(include "./unix-utility.scm")
(include "./jailman-utility.scm")
(include "./jailman-fetch-media.scm")
(include "./jailman-list-media.scm")
(include "./jailman-remove-media.scm")
(include "./jailman-create.scm")
(include "./jailman-start.scm")
(include "./jailman-stop.scm")
(include "./jailman-stopall.scm")
(include "./jailman-stopall.scm")
(include "./jailman-login.scm")
(include "./jailman-list.scm")
(include "./jailman-remove.scm")
(include "./jailman-help.scm")

(define *jailman-dir*
  (or
   (get-environment-variable "JAILMAN_BASE_DIR")
   "/usr/local/jailman/"))

(unless
    (char=? #\/
            (string-ref
             *jailman-dir*
             (- (string-length *jailman-dir*) 1)))
  (set! *jailman-dir*
        (string-append *jailman-dir* "/")))

(define-syntax define-jailman-dirs
  (syntax-rules ()
    [(_ base-dir (dir-name dir-path) ...)
     (begin
       (define dir-name
         (string-append
          base-dir
          dir-path )) ... )]))

(define-jailman-dirs
 *jailman-dir*
 (*jailman-media-dir* "media/")
 (*jailman-config-dir* "config/")
 (*jailman-container-dir* "container/")
 (*jailman-running-info-dir* "running-info/"))

(define (create-directories-when-not-exist . dirs)
  (cond ((null? dirs) 'nop)
        (else
         (unless (file-exists? (car dirs))
           (if (file-is-writable?
                (simplify-path
                 (string-append
                  (car dirs)
                  "..")))
               (begin
                 (make-directory* (car dirs))
                 (display
                  (string-append
                   "directory created: "
                   (car dirs) "\n")))
               (begin
                 (display "unwritable directory\n")
                 (exit 1))))
         (apply create-directories-when-not-exist (cdr dirs)))))

(unless (file-exists? *jailman-dir*)
  (if (ask-Yn
       (string-append
        "jailman creates a new directory: "
        *jailman-dir*))
      (create-directories-when-not-exist *jailman-dir*)
      (exit 1)))

(create-directories-when-not-exist
 *jailman-dir*
 *jailman-media-dir*
 *jailman-config-dir*
 *jailman-container-dir*
 *jailman-running-info-dir*)

(define (main args)
  (when (null? (cdr args))
    (display "'jailman help' shows available commands\n")
    (exit 1))
  (let ((command (cadr args))
        (sub-args (cddr args)))
    (cond ((string=? command "fetch-media")
           (jailman-check-permission 'media 'writable)
           (jailman-fetch-media sub-args))
          ((string=? command "remove-media")
           (jailman-check-permission 'media 'writable)
           (jailman-remove-media sub-args))
          ((string=? command "list-media")
           (jailman-check-permission 'media 'readable)
           (jailman-list-media sub-args))
          ((string=? command "create")
           (jailman-check-permission 'config 'writable)
           (jailman-check-permission 'container 'writable)
           (jailman-create sub-args))
          ((string=? command "start")
           (jailman-check-permission 'config 'readable)
           (jailman-check-permission 'container 'writable)
           (jailman-check-permission 'running-info 'writable)
           (jailman-start sub-args))
          ((string=? command "stop")
           (jailman-check-permission 'config 'readable)
           (jailman-check-permission 'container 'writable)
           (jailman-check-permission 'running-info 'writable)
           (jailman-stop sub-args))
          ((string=? command "stopall")
           (jailman-check-permission 'config 'readable)
           (jailman-check-permission 'container 'writable)
           (jailman-check-permission 'running-info 'writable)
           (jailman-stopall sub-args))
          ((string=? command "login")
           (jailman-check-permission 'container 'writable)
           (jailman-login sub-args))
          ((string=? command "list")
           (jailman-check-permission 'config 'readable)
           (jailman-check-permission 'container 'readable)
           (jailman-list sub-args))
          ((string=? command "remove")
           (jailman-check-permission 'config 'writable)
           (jailman-check-permission 'container 'writable)
           (jailman-remove sub-args))
          ((string=? command "help")
           (jailman-help))
          (else
           (display
            (string-append
             "unkown command: "
             command "\n" ))
           1 ))))

(main (command-line)) ;; defined in (scheme process-context)
