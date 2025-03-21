          ;; FreeBSD update
          (do-process
           `(freebsd-update -b ,(jailman-get-container-path name)
                            fetch install))
