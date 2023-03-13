;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-home)
  #:use-module (common)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (rosenthal packages emacs-xyz)
  #:use-module (rosenthal packages web)
  #:use-module (rosenthal packages wm)
  #:use-module (rosenthal services child-error)
  #:use-module (rosenthal utils counter-stop))

(load (summon "blob-dorphine-home.scm"))


;;
;; Dolly
;;


(define rofi-dolly
  (package-input-rewriting/spec
   `(("rofi" . ,(const rofi-wayland)))))

(define buku-run-dev/dolly
  (package-with-patches
   buku-run-dev
   (list
    ;; (fix #27) Show Bookmark tag correctly
    (origin
      (method url-fetch)
      (uri "https://github.com/carnager/buku_run/pull/29.patch")
      (sha256
       (base32
        "0j5f6nifa3ibhgcvfn59pq7xsnbcwgj6vkcz8vm4wway2nl5b9i6"))))))

(define pinentry-rofi/dolly
  (rofi-dolly pinentry-rofi))


;;
;; File-like
;;


(define %config-gpg-agent
  (mixed-text-file
   "gpg-agent.conf"
   "pinentry-program " (file-append pinentry-rofi/dolly "/bin/pinentry-rofi") "\n"))

(define %config-hyprland
  (let ((filename "hyprland.conf"))
    (mixed-text-file
     filename
     (agathion filename) "\n"

     ;; Monitors
     "monitor = , preferred, auto, 1" "\n"
     "monitor = HDMI-A-1, 1920x1080@60, 0x0, 1" "\n"
     "monitor = eDP-1, 2560x1600@120, 1920x0, 1.5, bitdepth,10" "\n\n"

     ;; Binds
     "bind = $mainMod, E, exec, emacsclient --create-frame --no-wait --alternate-editor=''" "\n"
     "bind = $mainMod, Q, exec, " (file-append foot "/bin/foot") "\n"
     "bind = $mainMod, B, exec, " (file-append buku-run-dev "/bin/buku_run") "\n"
     "bind = $mainMod, D, exec, " (file-append tessen "/bin/tessen") "\n"
     "bind = $mainMod, R, exec, " (file-append rofi-wayland "/bin/rofi") " -show combi" "\n\n"

     "bindle = , XF86MonBrightnessUp, exec, " (file-append light "/bin/light")" -A 5" "\n"
     "bindle = , XF86MonBrightnessDown, exec, " (file-append light "/bin/light") " -U 5" "\n\n"

     ;; Dispatchers
     "exec-once = " (file-append swaybg "/bin/swaybg") " --image " (nohitaga "94280741_p0.jpg") " --mode fill --output '*'" "\n"
     "exec-once = " (file-append wlsunset "/bin/wlsunset") " " %dorphine-wlsunset-args "\n"
     "exec = " (file-append hyprland "/bin/hyprctl") " setcursor Adwaita 24" "\n")))


;;
;; Inferior
;;


(define firefox
  (let* ((channels
          (list (channel
                 (inherit %channel-guix)
                 (commit "0bb0eeddf647e5d56afd4517b12919a36acac6ee"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "2dde2a60067ee383b753b85d608a7c20ff315634"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "110.0.1"))))


;;
;; Package bundles
;;


(define %home-packages-emacs
  (append
   (list emacs-rainbow-delimiters
         emacs-wanderlust
         emacs-yasnippet)
   (map (package-input-rewriting/spec
         `(("emacs-minimal" . ,(const emacs-next-pgtk))))
        (list emacs-ace-jump-mode
              emacs-ace-link
              emacs-apheleia
              emacs-browse-kill-ring
              emacs-company-dev
              emacs-ctrlf
              emacs-daemons
              emacs-doom-modeline-dev
              emacs-flycheck
              emacs-gcmh
              emacs-geiser-guile
              emacs-god-mode
              emacs-hl-todo
              emacs-macrostep-geiser
              emacs-mwim
              emacs-no-littering
              emacs-orderless
              emacs-org-modern
              emacs-org-rainbow-tags-dev
              emacs-ox-hugo
              emacs-pass
              emacs-password-store-otp
              emacs-puni
              emacs-rime-dev
              emacs-straight-el
              emacs-vertico
              emacs-visual-fill-column
              emacs-volatile-highlights-dev
              emacs-vundo
              emacs-wakatime-mode
              emacs-which-key))))

(define %home-packages
  (append (list emacs-next-pgtk
                buku
                exa
                firefox
                fish
                git
                git-crypt
                gnupg
                grimblast
                hyfetch
                hyprland
                isync
                libfido2
                mosh
                netcat-openbsd          ;for `-X` option
                openssh-sans-x
                pass-otp
                password-store
                python-prompt-toolkit
                rofi-wayland
                rsync
                wl-clipboard
                xdg-utils
                xonsh
                zathura
                zathura-pdf-poppler
                zoxide)
          (list adwaita-icon-theme
                hicolor-icon-theme)
          (list font-adobe-source-serif-pro
                font-chiron-hei-hk
                font-chiron-sung-hk
                font-google-noto
                font-lxgw-wenkai-tc
                font-sarasa-gothic
                font-victor-mono)
          %home-packages-emacs))


;;
;; Home environment definition for Dorphine.
;;


(home-environment
 (packages (map normalize-package %home-packages))
 (services
  (list (service home-channels-service-type
                 %rosenthal-default-channels)

        (service home-mcron-service-type
                 (home-mcron-configuration
                  (jobs (list #~(job next-hour-from
                                     #$(file-append modprobed-db "/bin/modprobed-db storesilent"))))))

        (service home-openssh-service-type
                 (home-openssh-configuration
                  (hosts %dorphine-ssh-hosts)))

        (service home-wakapi-service-type
                 (home-wakapi-configuration
                  (config %dorphine-wakapi-config)))

        (simple-service 'setup-xdg-config-home
                        home-xdg-configuration-files-service-type
                        `(("emacs/early-init.el" ,(nohitaga "emacs-early-init.el"))
                          ("emacs/init.el" ,(nohitaga "emacs-init.el"))
                          ("foot/foot.ini" ,(nohitaga "foot.ini"))
                          ("git/config" ,(nohitaga "git.conf"))
                          ("hypr/hyprland.conf" ,%config-hyprland)
                          ("isync/mbsyncrc" ,(nohitaga "mbsync.conf"))
                          ("mpv/mpv.conf" ,(nohitaga "mpv.conf"))
                          ("wanderlust/folders" ,(nohitaga "wanderlust-folders.conf"))
                          ("wanderlust/init.el" ,(nohitaga "wanderlust-init.el"))))

        (simple-service 'setup-xdg-data-home
                        home-xdg-data-files-service-type
                        `(("gnupg/gpg-agent.conf" ,%config-gpg-agent)))

        (simple-service 'setup-env-vars
                        home-environment-variables-service-type
                        `(,@%xdg-base-directory-environment-variables
                          ("BROWSER" . "firefox")
                          ("EDITOR" . "emacsclient")
                          ("VISUAL" . "$EDITOR"))))))
