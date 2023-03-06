(define-module (hugal31 packages guile)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages serialization)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public guile-ggspec
  (package
    (name "guile-ggspec")
    (version "1.3.1-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Hugal31/ggspec.git")
                    (commit "e1d50718a66ae3d3c3b85dc4c8d4b1d10d3c7c49")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a8yq4x2klqnzad96c0jy8168qw5x4nmd8zpdgpsiwcwnas1na03"))))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'delete-tests
                    (lambda _
                      (delete-file-recursively "spec")))
                  (add-before 'build 'move-exec
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (mkdir-p bin)
                        (copy-file "ggspec"
                                   (string-append bin "/ggspec")))
                      (delete-file "ggspec")))
                  (add-after 'move-exec 'move-module
                    (lambda _
                      (mkdir "ggspec")
                      (rename-file "lib.scm" "ggspec/lib.scm"))))))
    (build-system guile-build-system)
    (inputs (list guile-3.0))
    (home-page "https://github.com/yawaramin/ggspec")
    (synopsis
     "A lightweight, functional-style unit testing framework for Guile/Scheme")
    (description
     "ggspec is a very lightweight unit testing framework for Guile. Currently I am targeting Guile 1.8. I may port it to other Schemes in future; in principle it should be fairly simple because it doesn't use very many Guile-specific features.")
    (license license:expat)))

(define-public guile-libyaml
  (let ((commit "f5d33a6880e96571d3cb079ed7755ffc156cac46")
        (revision "1"))
    (package
      (name "guile-libyaml")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mwette/guile-libyaml")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12x91983fh1j39zy7kbk19acc1rqdh8515ddx1mh7l26j04k9wgq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules (((guix build guile-build-system)
                     #:prefix guile:)
                    ,@%gnu-build-system-modules)
         #:imported-modules ((guix build guile-build-system)
                             ,@%gnu-build-system-modules)
         #:tests? #f ;there are none
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (add-after 'unpack 'remove-unused-files
                      (lambda* (#:key inputs #:allow-other-keys)
                        (for-each delete-file
                                  '("guix.scm" "demo1.yml" "demo1.scm"))
                        ;; (copy-file (string-append (assoc-ref inputs "nyacc")
                        ;; "/share/guile/site/3.0/system/ffi-help-rt.scm")
                        ;; "yaml/ffi-help-rt.scm")
                        ;; (substitute* "yaml/ffi-help-rt.scm"
                        ;; (("system ffi-help-rt") "yaml ffi-help-rt"))
                        #t))
                    (add-before 'build 'build-ffi
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; (invoke "guild" "compile-ffi"
                        ;; "--no-exec" ; allow us to patch the generated file
                        ;; "yaml/libyaml.ffi")
                        (substitute* "yaml/libyaml.scm"
                          ;; (("system ffi-help-rt") "yaml ffi-help-rt")
                          (("dynamic-link \"libyaml\"")
                           (format #f "dynamic-link \"~a/lib/libyaml\""
                                   (assoc-ref inputs "libyaml")))) #t))
                    (replace 'build
                      (assoc-ref guile:%standard-phases
                                 'build))
                    (delete 'install))))
      (inputs `(("guile" ,guile-3.0-latest)
                ("libyaml" ,libyaml)))
      (propagated-inputs `(("guile-bytestructures" ,guile-bytestructures)
                           ;; ("nyacc" ,nyacc)
                           ))
      ;; (native-inputs
      ;; `(("nyacc" ,nyacc)))
      (home-page "https://github.com/mwette/guile-libyaml")
      (synopsis "Guile wrapper for libyaml")
      (description
       "This package provides a simple yaml module for Guile using the
ffi-helper from nyacc.")
      (license license:lgpl3+))))
