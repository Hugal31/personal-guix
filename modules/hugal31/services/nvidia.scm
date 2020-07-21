(define-module (hugal31 services nvidia)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (hugal31 packages nvidia)
  #:export (nvidia-insmod-service-type))

(define-configuration nvidia-insmod-configuration
  (package
    (package nvidia-driver-nonfree)
    "The nvidia-driver package to use."))

(define (nvidia-insmod-shepherd-service configs)
  (map (match-lambda
         (($ <nvidia-insmod-configuration> package)
          (shepherd-service
           (provision '(nvidia-insmod))
           (requirement '())
           ;; run the nvidia-insmod script
           (start #~(lambda _
                      ;; TODO What does the "and" is used for?
                      (and
                       (zero? (system* #$(string-append package "/bin/nvidia-insmod"))))))
           (one-shot? #t)
           (auto-start? #t)
           (respawn? #f))))
       configs))

(define nvidia-insmod-service-type
  (service-type
   (name 'nvidia-insmod-name)
   (extensions
    (list (service-extension shepherd-root-service-type
                             nvidia-insmod-shepherd-service)))

   (default-value '())
   ;; TODO What?
   ;; (compose concatenate)
   ;; (extend append)
   ))
