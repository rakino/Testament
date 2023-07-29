;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-home)
  #:use-module ((testament common)
                #:select (testament-find-file
                          (testament-file-content . agathion)
                          (testament-file-object . nohitaga)))
  #:use-module (testament counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages haskell-apps)
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
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages emacs-xyz)
  #:use-module (rosenthal packages gnome-xyz)
  #:use-module (rosenthal packages tree-sitter)
  #:use-module (rosenthal packages web)
  #:use-module (rosenthal packages wm)
  #:use-module (rosenthal services child-error))

(load (testament-find-file "blob-dorphine"))


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

(define emacs-doom-modeline/dolly
  (let ((base emacs-doom-modeline))
    (package
      (inherit base)
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (delete "emacs-all-the-icons" "emacs-dash"))))))

(define emacs-helpful/dolly
  (let ((base emacs-helpful))
    (package
      (inherit base)
      (propagated-inputs '())
      (inputs (package-propagated-inputs base)))))

(define gopls/dolly
  (let ((base gopls))
    (package
      (inherit base)
      (propagated-inputs '())
      (inputs (package-propagated-inputs base)))))

(define pinentry-rofi/dolly
  (rofi-dolly pinentry-rofi))

(define tree-sitter-cmake/dolly
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "cmake" "CMake"
   "071q1ds7whish8mz11ba47gngv6yfhhf27vdkp31rs8kxnbxqzmd"
   "0.3.0"                              ;FIXME: 0.4.0 won't work on Emacs.
   #:repository-url "https://github.com/uyha/tree-sitter-cmake"))


;;
;; File-like
;;


(define %config-gpg-agent
  (let ((pinentry-rofi (file-append pinentry-rofi/dolly "/bin/pinentry-rofi")))
    (mixed-text-file
     "gpg-agent.conf"
     "pinentry-program " pinentry-rofi "\n")))

(define %config-hyprland
  (let ((filename   "hyprland.conf")
        (wallpaper (nohitaga "94280741_p0.jpg"))
        (lockpaper (nohitaga "102982564_p0.jpg"))
        (alacritty (file-append alacritty "/bin/alacritty"))
        (buku_run  (file-append buku-run-dev/dolly "/bin/buku_run"))
        (hyprctl   (file-append hyprland "/bin/hyprctl"))
        (light     (file-append light "/bin/light"))
        (rofi      (file-append rofi-wayland "/bin/rofi"))
        (swaybg    (file-append swaybg "/bin/swaybg"))
        (swayidle  (file-append swayidle "/bin/swayidle"))
        (swaylock  (file-append swaylock-effects "/bin/swaylock"))
        (tessen    (file-append tessen "/bin/tessen"))
        (wlsunset  (file-append wlsunset "/bin/wlsunset")))
    (mixed-text-file
     filename
     (agathion filename) "\n"

     ;; Monitors
     "monitor = , preferred, auto, 1\n"
     "monitor = HDMI-A-1, preferred, auto, 1\n"
     "monitor = eDP-1, preferred, auto, 1.5\n\n"

     ;; Binds
     "bind = $mainMod, E, exec, emacsclient --create-frame"
                                          " --no-wait"
                                          " --alternate-editor=''\n"
     "bind = $mainMod, Q, exec, " alacritty "\n"
     "bind = $mainMod, B, exec, " buku_run "\n"
     "bind = $mainMod, D, exec, " tessen "\n"
     "bind = $mainMod, R, exec, " rofi " -show combi\n"
     "bind = $mainMod, L, exec, " swaylock " --clock -fei " lockpaper "\n\n"

     "bindl = , switch:on:Lid Switch, exec, " hyprctl " dispatch dpms off eDP-1\n"
     "bindl = , switch:off:Lid Switch, exec, " hyprctl " dispatch dpms on eDP-1\n\n"

     "bindle = , XF86MonBrightnessUp, exec, " light " -A 5\n"
     "bindle = , XF86MonBrightnessDown, exec, " light " -U 5\n\n"

     ;; Dispatchers
     "exec-once = " swaybg " --image " wallpaper " --mode fill --output '*'\n"
     "exec-once = " swayidle " -w"
                  " timeout 300 '" swaylock " --clock -fei " lockpaper "'"
                  " timeout 600 '" hyprctl " dispatch dpms off'"
                  " resume '" hyprctl " dispatch dpms on'\n"
     "exec-once = " wlsunset " " %dorphine-wlsunset-args "\n\n"

     "exec = " hyprctl " setcursor Qogir 24\n")))

(define %config-wget
  (plain-file
   "wgetrc"
   (format #f "hsts-file = ~a/wget-hsts~%"
           (getenv "XDG_CACHE_HOME"))))

(define %shell-profile-wm
  (plain-file "shell-profile-wm"
              (format #f "~
if [ -z \"${WAYLAND_DISPLAY}\" ] && [ \"${XDG_VTNR}\" -eq 1 ]; then
    exec Hyprland
fi")))


;;
;; Inferior
;;


(define firefox
  (let* ((channels
          (list (channel
                 (inherit %channel-guix)
                 (commit "15f9870eb36e688fac2af37828971779b6c56916"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "27e966f58680ede69abded71167348e954d40279"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "115.0.2"))))


;;
;; Package bundles
;;


(define %home-packages-emacs
  (append
   (list emacs-helpful/dolly
         emacs-rainbow-delimiters
         emacs-wanderlust
         emacs-yasnippet)
   (map (package-input-rewriting/spec
         `(("emacs-minimal" . ,(const emacs-next-pgtk))))
        (list emacs-apheleia
              emacs-company
              emacs-ctrlf
              emacs-denote
              emacs-doom-modeline/dolly
              emacs-eldoc-box
              emacs-elisp-refs
              emacs-gcmh
              emacs-geiser-guile
              emacs-god-mode
              emacs-guix
              emacs-hl-todo
              emacs-macrostep-geiser
              emacs-magit
              emacs-markdown-mode
              emacs-mwim
              emacs-no-littering
              emacs-orderless
              emacs-org-modern
              emacs-org-rainbow-tags
              emacs-ox-hugo
              emacs-pass
              emacs-password-store-otp
              emacs-puni
              emacs-rime
              emacs-spamfilter-el
              emacs-vertico
              emacs-visual-fill-column
              emacs-volatile-highlights
              emacs-wakatime-mode
              emacs-which-key
              emacs-xonsh-mode))))

(define %home-packages
  (append (list emacs-next-pgtk
                buku
                exa
                firefox
                git
                `(,git "send-email")
                git-crypt
                gnupg
                grimblast
                hyfetch
                hyprland
                libfido2
                mosh
                openssh-sans-x
                pass-otp
                password-store
                python-prompt-toolkit
                rofi-wayland
                rsync
                wakatime-cli-bin
                wl-clipboard
                xdg-desktop-portal
                xdg-desktop-portal-wlr
                xdg-utils
                xonsh
                zathura
                zathura-pdf-poppler
                zoxide)
          (list adwaita-icon-theme
                hicolor-icon-theme
                qogir-icon-theme)
          (list font-adobe-source-serif-pro
                font-chiron-hei-hk
                font-chiron-sung-hk
                font-google-noto
                font-lxgw-wenkai-tc
                font-sarasa-gothic
                font-victor-mono)
          (list tree-sitter-bash
                tree-sitter-c
                tree-sitter-cmake/dolly
                tree-sitter-cpp
                tree-sitter-c-sharp
                tree-sitter-css
                tree-sitter-dockerfile
                tree-sitter-go
                tree-sitter-gomod
                tree-sitter-java
                tree-sitter-javascript
                tree-sitter-json
                tree-sitter-python
                tree-sitter-ruby
                tree-sitter-rust
                tree-sitter-typescript
                tree-sitter-yaml)
          (list ccls
                gcc-toolchain-12
                go-1.20
                gopls/dolly
                shellcheck)
          %home-packages-emacs))


;;
;; Home environment definition for Dorphine.
;;


(home-environment
 (packages (map normalize-package %home-packages))
 (services
  (list (service home-channels-service-type
                 %testament-default-channels)

        (service home-dbus-service-type)

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

        (simple-service 'setup-env-vars
                        home-environment-variables-service-type
                        `(,@%xdg-base-directory-environment-variables
                          ("BROWSER" . "firefox")
                          ("EDITOR" . "emacsclient")
                          ("GUILE_AUTO_COMPILE" . "0")
                          ("VISUAL" . "$EDITOR")))

        (simple-service 'setup-non-xdg-home
                        home-files-service-type
                        `((".icons/default/index.theme" ,(nohitaga "icons.theme"))))

        (simple-service 'setup-xdg-config-home
                        home-xdg-configuration-files-service-type
                        `(("alacritty/alacritty.yml" ,(nohitaga "alacritty.yml"))
                          ("emacs/early-init.el" ,(nohitaga "emacs-early-init.el"))
                          ("emacs/init.el" ,(nohitaga "emacs-init.el"))
                          ("git/config" ,(nohitaga "git.conf"))
                          ("gtk-3.0/settings.ini" ,(nohitaga "gtk-3.0.ini"))
                          ("hypr/hyprland.conf" ,%config-hyprland)
                          ("mpv/mpv.conf" ,(nohitaga "mpv.conf"))
                          ("npm/npmrc" ,(nohitaga "npm.conf"))
                          ("pythonstartup.py" ,(nohitaga "pythonstartup.py"))
                          ("wanderlust/folders" ,(nohitaga "wanderlust-folders.conf"))
                          ("wanderlust/init.el" ,(nohitaga "wanderlust-init.el"))
                          ("wgetrc" ,%config-wget)
                          ("xonsh/rc.xsh" ,(nohitaga "xonsh.xsh"))))

        (simple-service 'setup-xdg-data-home
                        home-xdg-data-files-service-type
                        `(("gnupg/gpg-agent.conf" ,%config-gpg-agent)))

        (simple-service 'setup-shell-profile
                        home-shell-profile-service-type
                        (list %shell-profile-wm)))))
