;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (gnu system file-systems)
  #:export (%channel-guix
            %channel-guixcn
            %channel-nonguix
            %channel-rosenthal

            %guix-authorized-key-dorphine
            %guix-authorized-key-nonguix

            %testament-xdg-base-directory-env-vars
            %testament-base-file-systems))


;;
;; Channels
;;


(define %channel-guix
  (first %default-channels))

(define %channel-guixcn
  (channel
   (name 'guixcn)
   (url "https://github.com/guixcn/guix-channel.git")
   (introduction
    (make-channel-introduction
     "993d200265630e9c408028a022f32f34acacdf29"
     (openpgp-fingerprint
      "7EBE A494 60CE 5E2C 0875  7FDB 3B5A A993 E1A2 DFF0")))))

(define %channel-nonguix
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define %channel-rosenthal
  (channel
   (name 'rosenthal)
   (url "https://codeberg.org/hako/rosenthal.git")
   (branch "trunk")
   (introduction
    (make-channel-introduction
     "7677db76330121a901604dfbad19077893865f35"
     (openpgp-fingerprint
      "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7")))))


;;
;; Keys
;;


(define %guix-authorized-key-dorphine
  (plain-file "dorphine.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #7B44CACBD0FD38E2A083E0694441C230680BC6E78A407C74ACD23AF693A48BF5#)))"))

;; https://substitutes.nonguix.org/signing-key.pub
(define %guix-authorized-key-nonguix
  (plain-file "nonguix.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))


;;
;; Variables
;;


;; Source: <https://wiki.archlinux.org/title/XDG_Base_Directory>
(define %testament-xdg-base-directory-env-vars
  '(;; XDG Cache Home
    ("GOMODCACHE" . "$XDG_CACHE_HOME/go/mod")
    ("PYTHONPYCACHEPREFIX" . "$XDG_CACHE_HOME/python")

    ;; XDG Config Home
    ("NPM_CONFIG_USERCONFIG" . "$XDG_CONFIG_HOME/npm/npmrc")
    ("PYTHONSTARTUP" . "$XDG_CONFIG_HOME/pythonstartup.py")
    ("WAKATIME_HOME" . "$XDG_CONFIG_HOME/wakatime")
    ("WGETRC" . "$XDG_CONFIG_HOME/wgetrc")

    ;; XDG Data Home
    ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
    ("GDBHISTFILE" . "$XDG_DATA_HOME/gdb/history")
    ("GOPATH" . "$XDG_DATA_HOME/go")
    ("GUILE_HISTORY" . "$XDG_DATA_HOME/guile/history")
    ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/pass")
    ("PYTHONUSERBASE" . "$XDG_DATA_HOME/python")))

(define %testament-base-file-systems
  (cons* (file-system
           (device "tmpfs")
           (mount-point "/tmp")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev strict-atime))
           (options "nr_inodes=1m,size=60%"))

         (file-system
           (device "tmpfs")
           (mount-point "/run")
           (type "tmpfs")
           (needed-for-boot? #t)
           (check? #f)
           (flags '(no-dev strict-atime))
           (options "mode=0755,nr_inodes=800k,size=20%"))

         (file-system
           (device "tmpfs")
           (mount-point "/var/run")
           (type "tmpfs")
           (needed-for-boot? #t)
           (check? #f)
           (flags '(no-suid no-dev strict-atime))
           (options "mode=0755,nr_inodes=800k,size=20%"))

         (file-system
           (device "tmpfs")
           (mount-point "/var/lock")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev strict-atime))
           (options "nr_inodes=800k,size=20%"))

         (delete %debug-file-system
                 %base-file-systems)))
