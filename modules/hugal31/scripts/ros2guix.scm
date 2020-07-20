(define-module (hugal31 scripts ros2guix)
  #:use-module (guix scripts)
  #:use-module (srfi srfi-37)
  #:export (ros2guix-main))

(define (show-help)
  "Show help"
  (display "Usage: ros2guix [OPTION] PACKAGES...
Convert the given PACKAGES.\n")
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version information and exit"))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
	    (lambda _
	      (show-help)))))

(define (ros2guix-main args)
  (display args)
  (newline))
