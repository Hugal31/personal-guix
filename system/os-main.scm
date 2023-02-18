(define-module (os-main)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system trivial)
  #:use-module (guix diagnostics)
  #:use-module (guix download)
  #:use-module (guix transformations)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  ;#:use-module (hugal31 packages nvidia)
  ;#:use-module (hugal31 services nvidia)
  ;#:use-module (hugal31 system linux-nonfree)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu system linux-initrd))

(use-package-modules avahi
                     gl
                     gnome
                     libusb
                     screen
                     ssh
                     video
                     xdisorg
                     xfce
                     xorg
                     wm)
(use-service-modules avahi
                     dbus
                     desktop
                     networking
                     ssh
                     xorg)

;; TODO: Take configuration
(define (acpid-shepherd-service config)
  (list (shepherd-service
         (provision '(acpid))
         (requirement '())
         (modules %default-modules)
         (start #~(make-forkexec-constructor
                   ;; TODO: Property create the directory
                   (begin (mkdir-p "/etc/acpi/events")
                          (list (string-append #$acpid "/sbin/acpid") "--debug" "--logevents" "--foreground"))))
         (stop #~(make-kill-destructor)))))

(define acpid-service-type
  (service-type
   (name 'acpid)
   (description "Run the ACPI Daemon")
   (extensions
    (list (service-extension shepherd-root-service-type acpid-shepherd-service)))
   (default-value '())))

;; nvidia-driver with ne same number of char than mesa (with -version appended),
;; because grafted replacement must have the same lenght.
;; mesa-20.0.7
;; nvdr-435.21
;; (define nvidia-driver
;;   (package
;;     (inherit nvidia-driver-nonfree)
;;     (name "nvdr")
;;     (propagated-inputs
;;      `(;; Inputs from mesa
;;         ("libdrm" ,libdrm)
;;         ("libvdpau" ,libvdpau)
;;         ("libx11" ,libx11)
;;         ("libxdamage" ,libxdamage)
;;         ("libxfixes" ,libxfixes)
;;         ("libxshmfence" ,libxshmfence)
;;         ("libxxf86vm" ,libxxf86vm)
;;         ("xorgproto" ,xorgproto)
;;         ("mesa-headers" ,mesa-headers)
;;         ,@(package-propagated-inputs nvidia-driver-nonfree)))))

;; (define %replace-mesa-input-by-nvidia
;;   (package-input-rewriting
;;    `((,mesa . ,nvidia-driver))))

;; (define %graft-mesa-input-by-nvidia
;;   (package-input-rewriting
;;    `((,mesa . ,(package/inherit mesa
;;                                 (replacement nvidia-driver))))))

(define replace-mesa-by-nvda
  (options->transformation
   '((with-graft . "mesa=nvda"))))

(define xorg-server-nvidia (replace-mesa-by-nvda xorg-server))

;;(define patched-xfce (%replace-mesa-input-by-nvidia xfce))

(define %kernel linux-lts)

;; TODO Do not hardcode path to nvidia-smi, etc...
(define %nvidia-udev-rule
  (udev-rule
   "71-nvidia.rules"
   "# Tag the device as master-of-seat so that logind is happy
# (see LP: #1365336)
SUBSYSTEM==\"pci\", ATTRS{vendor}==\"0x10de\", DRIVERS==\"nvidia\", TAG+=\"seat\", TAG+=\"master-of-seat\"

# Start and stop nvidia-persistenced on power on and power off
# respectively
ACTION==\"add\", DEVPATH==\"/bus/pci/drivers/nvidia\", TAG+=\"systemd\", ENV{SYSTEMD_WANTS}=\"nvidia-persistenced.service\"

# Load and unload nvidia-modeset module
ACTION==\"add\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe nvidia-modeset\"
ACTION==\"remove\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe -r nvidia-modeset\"

# Load and unload nvidia-drm module
ACTION==\"add\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe nvidia-drm\"
ACTION==\"remove\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe -r nvidia-drm\"

# Load and unload nvidia-uvm module
ACTION==\"add\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe nvidia-uvm\"
ACTION==\"remove\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/run/current-system/profile/bin/modprobe -r nvidia-uvm\"

# This will create the device nvidia device nodes
ACTION==\"add\", DEVPATH==\"/bus/pci/drivers/nvidia\", RUN+=\"/usr/bin/nvidia-smi\"

# Create the device node for the nvidia-uvm module
ACTION==\"add\", DEVPATH==\"/module/nvidia_uvm\", SUBSYSTEM==\"module\", RUN+=\"/sbin/create-uvm-dev-node\"
"))

(define-public %extra-packages
  (list acpi
        nss-certs
	nvidia-driver
                                        ;patched-xfce
        i3-gaps
	zsh))

(define %extra-services
  (list
   ;; TODO Make this an user service ?
   ;; (service autossh-service-type
   ;;          (autossh-configuration
   ;;           (user "laloge_h")
   ;;           (ssh-options (list "-M" "22223" "-N" "kimsufi" "-R" "22222:localhost:22"))))
   (service acpid-service-type)
   (service openssh-service-type)
   ;(service nvidia-insmod-service-type)
   ;; (service xfce-desktop-service-type
   ;;          (xfce-desktop-configuration
   ;;           (xfce patched-xfce)))
   ;;(udev-rules-service 'nvidia %nvidia-udev-rule)
   (simple-service 'nvidia-udev-rules udev-service-type
                   (list nvidia-driver))
   ))

(define %xorg-modules
  ;; Default list of modules loaded by the server.  When multiple drivers
  ;; match, the first one in the list is loaded.
  (map replace-mesa-by-nvda
                                        ;%graft-mesa-input-by-nvidia
       (list
        nvidia-driver
        xf86-video-vesa
        xf86-video-fbdev
                                        ;xf86-video-amdgpu
        xf86-video-ati
        xf86-video-cirrus
        xf86-video-intel
        xf86-video-mach64
                                        ;xf86-video-nouveau
        xf86-video-nv
        xf86-video-sis

        ;; Libinput is the new thing and is recommended over evdev/synaptics:
        ;; <http://who-t.blogspot.fr/2015/01/xf86-input-libinput-compatibility-with.html>.
        xf86-input-libinput

        xf86-input-evdev
        xf86-input-keyboard
        xf86-input-mouse
        xf86-input-synaptics)))

(define %desktop-services-with-nvidia
  (list
   (service gdm-service-type
            (gdm-configuration
             (gdm (replace-mesa-by-nvda gdm))
             (xorg-configuration (xorg-configuration
                                  (modules (filter
                                            (lambda (p)
                                              (member (%current-system)
                                                      (package-supported-systems p)))
                                            %xorg-modules))
                                  (drivers '("nvidia"))
                                  (server xorg-server-nvidia)))))

   ;; Screen lockers are a pretty useful thing and these are small.
   ;;(screen-locker-service slock)
   ;;(screen-locker-service xlockmore "xlock")

   ;; Add udev rules for MTP devices so that non-root users can access
   ;; them.
   (simple-service 'mtp udev-service-type (list libmtp))
   ;; Add udev rules for scanners.
   ;;(service sane-service-type)
   ;; Add polkit rules, so that non-root users in the wheel group can
   ;; perform administrative tasks (similar to "sudo").
   polkit-wheel-service

   ;; The global fontconfig cache directory can sometimes contain
   ;; stale entries, possibly referencing fonts that have been GC'd,
   ;; so mount it read-only.
   fontconfig-file-system-service

   ;; NetworkManager and its applet.
   (service network-manager-service-type)
   (service wpa-supplicant-service-type)    ;needed by NetworkManager
   ;; (simple-service 'network-manager-applet
   ;;                 profile-service-type
   ;;                 (list network-manager-applet))
   (service modem-manager-service-type)
   (service usb-modeswitch-service-type)

   ;; The D-Bus clique.
   (service avahi-service-type)
   (udisks-service)
   (service upower-service-type)
   (accountsservice-service)
   (service cups-pk-helper-service-type)
   (service colord-service-type)
   (geoclue-service)
   (service polkit-service-type)
   (elogind-service)
   (dbus-service)

   (service ntp-service-type)

   x11-socket-directory-service

                                        ;(service pulseaudio-service-type)
                                        ;(service alsa-service-type)
   ))

(define %services
  (append
   %extra-services
   %desktop-services-with-nvidia
   (modify-services %base-services
      (guix-service-type config => (guix-configuration
                                    (inherit config)
                                    (substitute-urls
                                     (append (list "https://substitutes.nonguix.org")
                                             %default-substitute-urls))
                                    (authorized-keys
                                     (append (list (local-file "./nonguix-signing-key.pub"))
                                             %default-authorized-guix-keys)))))))

;; Modules to add to load at boot
(define %extra-linux-modules
  '(
    ;"nvidia"
    ;"nvidia_drm"
    ;"nvidia_uvm"
    ;"nvidia_modeset"
                                        ;"fuse"                      ; for sshfs
                                        ;"nbd"                       ; to mount qcow2 images
                                        ;"sata_nv"                   ; for my HDD to be recognized
                                        ;"snd-seq"                   ; for MIDI-keyboard
    ))

;; Modules to NOT load at boot
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
		(device "/dev/sda6")
		(mount-point "/")
		(type "ext4")
                (flags '(no-atime))))

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
  (kernel-loadable-modules (list nvidia-driver))

  (kernel-arguments
   (list
    "nvidia-drm.modeset=1"
    (string-append "modprobe.blacklist="
                   (list->comma-separated
                    %redundant-linux-modules))))

  (initrd microcode-initrd)
  (initrd-modules (append %extra-linux-modules %base-initrd-modules))

  (firmware (list linux-firmware)
   ;; (cons linux-firmware-non-free
	    ;;       %base-firmware)
            )

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
  (services
   (cons*
    (modify-services %services
      (guix-service-type config => (guix-configuration
                                    (inherit config)
                                    (substitute-urls
                                     (append (list "https://substitutes.nonguix.org")
                                             %default-substitute-urls))
                                    (authorized-keys
                                     (append (list (local-file "./nonguix-signing-key.pub"))
                                             %default-authorized-guix-keys))))))))
