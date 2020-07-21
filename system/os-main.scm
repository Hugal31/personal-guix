(define-module (os-main)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (hugal31 packages nvidia)
  #:use-module (hugal31 services nvidia)
  #:use-module (hugal31 system linux-nonfree)
  #:use-module (srfi srfi-1))

(use-service-modules desktop networking ssh)
(use-package-modules screen ssh xfce)

(define %kernel linux-nonfree)

(define-public %extra-packages
  (list nss-certs
	nvidia-driver-nonfree
	xfce
	zsh))

(define %extra-services
  (list
   ;; (service dhcp-client-service-type)
   (service provenance-service-type)
   (service nvidia-insmod-service-type)
   (service openssh-service-type)))

(define %extra-linux-modules
  '(;"fuse"                      ; for sshfs
    ;"nbd"                       ; to mount qcow2 images
    ;"sata_nv"                   ; for my HDD to be recognized
    ;"snd-seq"                   ; for MIDI-keyboard
    ))

(define %redundant-linux-modules
  '("amd64_edac_mod"
    "pcspkr" ;; "snd_pcsp"
    "nouveau"))

(define (list->comma-separated lst)
  (string-join lst ","))

(define no-op-installer
  #~(lambda (bootloader device mount)
      (format #t "Do not install bootloader ~a at ~a on ~a" bootloader device mount)
      (newline)))

;; Mount points
(define %root (file-system
		(device (file-system-label "GuixOS"))
		(mount-point "/")
		(type "ext4")))

(define %other-home (file-system
		      (device "/dev/sdb6")
		      (mount-point "/media/other_home")
		      (type "ext4")
		      (flags '(no-atime))
		      (create-mount-point? #t)))

(operating-system
  (host-name "hugo-guix-MS-7C37")
  (timezone "Europe/Paris")
  (locale "fr_FR.utf8")

  (kernel %kernel)

  (kernel-arguments
   (list (string-append "modprobe.blacklist="
			(list->comma-separated
			       %redundant-linux-modules))))

  (initrd-modules (append %extra-linux-modules %base-initrd-modules))

  (firmware (cons linux-firmware-non-free
		  %base-firmware))

  ;; Add grub which does not install itelf, but will be managed my another grub.
  (bootloader (bootloader-configuration
	       (bootloader (bootloader
			    (inherit grub-bootloader)
			    (installer no-op-installer)))))

  (file-systems (cons* %root
		       %other-home
		       %base-file-systems))
  (swap-devices '("/dev/sdb5"))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
		(name "laloge_h")
		(comment "Hugo Laloge")
		(group "laloge_h")
		(shell #~(string-append #$zsh "/bin/zsh"))

		;; Adding the account to the "wheel" group
		;; makes it a sudoer.  Adding it to "audio"
		;; and "video" allows the user to play sound
		;; and access the webcam.

		(supplementary-groups '("users" "wheel"
					"audio" "video")))
	       %base-user-accounts))
  (groups (cons (user-group
		 (name "laloge_h")
		 (id 1000))
		%base-groups))

  ;; Globally-installed packages.
  (packages (append %extra-packages %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append %extra-services %desktop-services)))
