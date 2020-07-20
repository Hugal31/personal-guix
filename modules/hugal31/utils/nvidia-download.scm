(define-module (hugal31 utils nvidia-download)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:export (url-fetch/nvidia-auto-decompressor))

;; TODO Make this more generic
(define* (url-fetch/nvidia-auto-decompressor url hash-algo hash
                                             #:optional name
                                             #:key (system (%current-system))
                                             (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the file from URL in a directory of its
own by executing it"
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (mlet %store-monad ((drv (url-fetch url hash-algo hash
                                      (string-append "decompressor-" (or name file-name))
                                      #:system system
                                      #:guile guile))
                      (guile (package->derivation guile system)))
    ;; Take the auto-decompressor and execute it
    (with-imported-modules '((guix build utils))
      (gexp->derivation (or name file-name)
                        #~(begin
                            (use-modules (guix build utils))
                                        ;(mkdir #$output)
                                        ;(chdir #$output)
                            ;; I am not sure this is a good method
                            (for-each
                             (lambda (package)
                               (setenv "PATH" (string-append package "/bin" ":" (getenv "PATH"))))
                             (list #+coreutils #+gawk #+grep #+tar #+which #+xz))
                            (invoke (string-append #+bash "/bin/sh") #$drv "--extract-only" "--target" #$output))
                        #:system system
                        #:guile-for-build guile
                        #:graft? #f
                        #:local-build? #t))))
