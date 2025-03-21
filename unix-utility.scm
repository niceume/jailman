(import (gauche process)
        (srfi 152))

(define (do-fetch url file-path)
  (do-process `(fetch ,url -o ,file-path)))

(define (get-current-arch)
  (process-output->string '(uname -m)))

(define (list-network-interfaces)
  (string-split
   (process-output->string '(ifconfig -l)) " "))

(define (string-list->do-process lst)
  (let* ((args '()))
    (for-each
     (lambda (x)
       (unless (or (string? x) (boolean? x))
         (display "ERROR\n")
         (display x)
         (display "ERROR\n")
         (error (string-append "ERROR: only string list is accepted "
                               "in string-list->do-process")))
       (if x
           (set! args
                 (append args (list (string->symbol x))))
           'nop))
     lst)
    (do-process args)))
