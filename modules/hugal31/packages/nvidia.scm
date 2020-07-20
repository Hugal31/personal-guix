(define-module (hugal31 packages nvidia)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system linux-module)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (hugal31 utils nvidia-download)
  #:use-module (hugal31 system linux-nonfree))

;; From https://github.com/lihebi/guix-channel
(define-public (make-nvidia-driver kernel)
  (package
    (name "nvidia-driver")
    (version "435.21")
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
		    version
		    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "0v3pq677ab01qdmwl5dawk8hn39qlwj05p8s9qzh9irmrlnc1izs"))
       (method url-fetch/nvidia-auto-decompressor)
       (patches (search-patches
                 "hugal31/packages/patches/nvidia/buildfix_kernel_5.4.patch"
                 "hugal31/packages/patches/nvidia/disable_fstack-clash-protection_fcf-protection.patch"))
       (file-name (string-append "nvidia-driver-" version "-checkout"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
    	 (replace 'build
    	   (lambda*  (#:key inputs outputs #:allow-other-keys)
    	     ;; I cannot use with-directory-excursion, because the install
    	     ;; phase needs to be in the kernel folder. Otherwise no .ko
    	     ;; would be installed
    	     (chdir "kernel")
    	     ;; patch Kbuild
    	     (substitute* "Kbuild"
    	       (("/bin/sh") (string-append (assoc-ref inputs "bash-minimal") "/bin/sh")))
    	     (invoke "make"
    		     "-j"
    		     (string-append "SYSSRC="
    				    ;; linux-libre "/lib/modules/5.2.13-gnu/source"
    				    (assoc-ref inputs "linux-module-builder")
    				    "/lib/modules/build")
    		     "CC=gcc")
    	     #t))
    	 (delete 'check)
    	 (add-after 'install 'install-copy
    	   (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
    	     (chdir "..")
    	     ;; for scandir
    	     (use-modules (ice-9 ftw)
    			  (ice-9 regex))
    	     (let* ((out (assoc-ref outputs "out"))
    		    (libdir (string-append out "/lib"))
    		    (bindir (string-append out "/bin")))
    	       ;; ------------------------------
    	       ;; Copy .so files
    	       (for-each
    		(lambda (file)
    		  (format #t "Copying '~a'...~%" file)
    		  (install-file file libdir))
    		(scandir "." (lambda (name)
    			       (string-contains name ".so"))))

    	       ;; xorg files
    	       (install-file "nvidia_drv.so" (string-append out "/lib/xorg/modules/drivers/"))
    	       (install-file "libglxserver_nvidia.so.435.21" (string-append out "/lib/xorg/modules/extensions/"))

    	       ;; ------------------------------
    	       ;; Binary files
    	       (install-file "nvidia-smi" bindir)

    	       ;; the runpath does not seem to work, thus the nvidia-xmi
    	       ;; (which is not patchelf-ed) intends to use system ld
    	       ;; (with LD_LIBRARY_PATH), and seems to work
    	       (copy-file "nvidia-smi" (string-append bindir "/nvidia-xmi"))

    	       ;; nvidia-settings cannot pass validate_runpath with weird errors
    	       ;; (install-file "nvidia-settings" bindir)

    	       ;; ------------------------------
    	       ;; Add a file to load nvidia drivers
    	       (let ((file (string-append bindir "/nvidia-insmod")))
    		 (call-with-output-file file
    		   (lambda (port)
    		     (display (string-append "#!" (assoc-ref inputs "bash-minimal") "/bin/sh" "\n") port)
    		     (display (string-append "modprobe ipmi_devintf" "\n") port)
    		     (display (string-append "export LINUX_MODULE_DIRECTORY="
    					     (string-append out "/lib/modules") "\n")
    			      port)
    		     (for-each (lambda (mod)
    				 (display (string-append "modprobe " mod "\n") port))
    			       ;; nvidia-drm cannot be loaded
    			       '("nvidia" "nvidia-modeset" "nvidia-uvm"))))
    		 (chmod file #o555))
    	       (let ((file (string-append bindir "/nvidia-rmmod")))
    		 (call-with-output-file file
    		   (lambda (port)
    		     (display (string-append "#!" (assoc-ref inputs "bash-minimal") "/bin/sh" "\n") port)
    		     (for-each (lambda (mod)
    				 (display (string-append "rmmod " mod "\n") port))
    			       '("nvidia-uvm" "nvidia-modeset" "nvidia" "ipmi_devintf"))))
    		 (chmod file #o555))

    	       ;; ------------------------------
    	       ;; patchelf
    	       (let* ((libc (assoc-ref inputs "libc"))
    		      (ld.so (string-append libc ,(glibc-dynamic-linker)))

    		      (out (assoc-ref outputs "out"))
    		      (rpath (string-join
    			      (list "$ORIGIN"
    				    (string-append out "/lib")
    				    (string-append libc "/lib")
    				    (string-append (assoc-ref inputs "libx11") "/lib")
    				    (string-append (assoc-ref inputs "libxext") "/lib")
    				    (string-append (assoc-ref inputs "pango") "/lib")
    				    (string-append (assoc-ref inputs "gtk+") "/lib")
    				    (string-append (assoc-ref inputs "gtk2") "/lib")
    				    (string-append (assoc-ref inputs "atk") "/lib")
    				    (string-append (assoc-ref inputs "glib") "/lib")
    				    (string-append (assoc-ref inputs "cairo") "/lib")
    				    (string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
    				    (string-append (assoc-ref inputs "wayland") "/lib")
    				    (string-append (assoc-ref inputs "gcc:lib") "/lib"))
    			      ":")))
    		 (define (patch-elf file)
		   ;; TODO: Here I temporarily change the mode of the file to modify it with patchelf.
		   ;; There must be a better solution
		   (let* ((saved-stat (stat file))
			  (saved-mode (stat:mode saved-stat))
			  (writable-mode (+ saved-mode #o200)))
		     (format #t "Patching ~a ...~%" file)
		     (chmod file writable-mode)
		     (unless (string-contains file ".so")
		       (invoke "patchelf" "--set-interpreter" ld.so file))
		     (invoke "patchelf" "--set-rpath" rpath file)
		     (chmod file saved-mode)))
    		 (for-each (lambda (file)
    			     (when (elf-file? file)
    			       (patch-elf file)))
    			   (find-files out  ".*\\.so"))
    		 ;; patch nvidia-smi but leave nvidia-xmi untouched
    		 (patch-elf (string-append out "/bin/nvidia-smi")))

    	       ;; ------------------------------
    	       ;; Create short name symbolic links
    	       (for-each (lambda (file)
    			   (let* ((short (regexp-substitute
    					  #f
    					  (string-match "([^/]*\\.so).*" file)
    					  1))
    				  (major (if (or (string=? short "libEGL.so")
    						 (string=? short "libEGL_nvidia.so")
    						 (string=? short "libGLX.so")
    						 (string=? short "libGLX_nvidia.so"))
    					     "0" "1"))
    				  (mid (string-append short "." major))
    				  (short-file (string-append libdir "/" short))
    				  (mid-file (string-append libdir "/" mid)))
    			     ;; FIXME the same name, print out warning at least
    			     ;; [X] libEGL.so.1.1.0
    			     ;; [ ] libEGL.so.435.21
    			     (when (not (file-exists? short-file))
    			       (format #t "Linking ~a to ~a ...~%" short file)
    			       (symlink (basename file) short-file))
    			     (when (not (file-exists? mid-file))
    			       (format #t "Linking ~a to ~a ...~%" mid file)
    			       (symlink (basename file) mid-file))))
    			 (find-files libdir "\\.so\\."))
    	       (symlink "libglxserver_nvidia.so.435.21"
    			(string-append out "/lib/xorg/modules/extensions/" "libglxserver_nvidia.so")))
    	     #t)))))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)))
    (inputs
     `(("atk" ,atk)
       ("bash-minimal" ,bash-minimal)
       ("cairo" ,cairo)
       ("gcc:lib" ,gcc "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk2" ,gtk+-2)
       ("kmod" ,kmod)
       ("libc" ,glibc)
       ("libx11" ,libx11)
       ("pango" ,pango)
       ("kernel" ,kernel)
       ("libxext" ,libxext)
       ("wayland" ,wayland)))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary Nvidia Driver")
    (description "Proprietary Nvidia Driver from Nvidia")
    (license (license:non-copyleft "https://www.nvidia.com/en-us/drivers/geforce-license/"
				   "License For Customer Use of NVIDIA GeForce Software"))))

(define-public nvidia-driver-nonfree
  (make-nvidia-driver linux-nonfree))
