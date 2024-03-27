;; SPDX-FileCopyrightText: 2022, 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-home)
  #:use-module (blobs dorphine)
  #:use-module (dorphine-emacs)
  #:use-module (testament common)
  #:use-module (testament counter-stop)
  #:use-module (testament packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services emacs)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal packages tree-sitter)
  #:use-module (rosenthal packages wm))


;;
;; File-like
;;


(define %alacritty-theme-catppuccin-latte
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/catppuccin/alacritty" "/raw/"
                        "832787d6cc0796c9f0c2b03926f4a83ce4d4519b"
                        "/catppuccin-latte.toml"))
    (sha256
     (base32 "09wvxg6vlfk4v47fildna1nxpjm88q6b7c6ig9xx1vyl5mpi8m51"))))

(define %config-alacritty
  (let ((filename "alacritty.toml"))
    (mixed-text-file
     filename
     "import = [ \"" %alacritty-theme-catppuccin-latte "\" ]\n"
     (testament-file-content filename) "\n")))

(define %hyprland-switch-keyboard-layout
  (program-file
   "switch-keyboard-layout.scm"
   (with-extensions (list guile-json-4)
     #~(begin
         (use-modules (ice-9 popen)
                      (json)
                      (rnrs io ports))

         (define (get-keyboards)
           (let* ((port (open-input-pipe
                         #$(file-append hyprland "/bin/hyprctl devices -j")))
                  (str (get-string-all port)))
             (close-pipe port)
             (assoc-ref (json-string->scm str) "keyboards")))

         (define (get-layout)
           (assoc-ref (vector-ref (get-keyboards) 0)
                      "active_keymap"))

         (define keyboards
           (get-keyboards))

         (define layout
           (if (string-contains (get-layout) "Dvorak")
               0
               1))

         (for-each
          (lambda (index)
            (system* #$(file-append hyprland "/bin/hyprctl")
                     "switchxkblayout"
                     (assoc-ref (vector-ref keyboards index)
                                "name")
                     (number->string (- 1 layout))))
          (iota (vector-length keyboards)))

         (system* #$(file-append libnotify "/bin/notify-send")
                  (string-append "Layout: " (get-layout)))))))

(define %config-hyprland
  (let ((filename  "hyprland.conf")
        (screenshot "~/Library/Pictures/Screenshots/$(date +%Y%m%d-%H%M%S).png")
        (wallpaper  (testament-file-object "96496968_p0.png"))
        (lock-args  #~(string-join
                       (list "--clock"
                             "--daemonize"
                             "--ignore-empty-password"
                             "--image"
                             #$(testament-file-object "115008128_p0.jpg"))))
        (alacritty  (file-append alacritty "/bin/alacritty"))
        (grimblast  (file-append grimblast "/bin/grimblast"))
        (hyprctl    (file-append hyprland "/bin/hyprctl"))
        (light      (file-append light "/bin/light"))
        (mako       (file-append mako "/bin/mako"))
        (rofi       (file-append rofi-wayland "/bin/rofi"))
        (swaybg     (file-append swaybg "/bin/swaybg"))
        (swaylock   (file-append swaylock-effects "/bin/swaylock"))
        (wlsunset   (file-append wlsunset "/bin/wlsunset"))
        (wpctl      (file-append wireplumber-minimal "/bin/wpctl"))
        (xdp        (file-append xdg-desktop-portal "/libexec/xdg-desktop-portal"))
        (xdp-gtk    (file-append xdg-desktop-portal-gtk "/libexec/xdg-desktop-portal-gtk"))
        (xdp-hypr   (file-append xdg-desktop-portal-hyprland "/libexec/xdg-desktop-portal-hyprland")))
    (mixed-text-file
     filename
     (testament-file-content filename) "\n"

     (apply string-append
            (append-map
             (match-lambda
               ((direction keys)
                (map (lambda (key)
                       (format #f "~
bind=SUPER,~a,movefocus,~a~%"
                               key direction))
                     keys)))
             '(("l" ("left"))
               ("d" ("down"))
               ("u" ("up"))
               ("r" ("right")))))

     (apply string-append
            (map (lambda (workspace-number)
                   (format #f "~
bind=SUPER,~a,workspace,~a
bind=SUPER SHIFT,~@*~a,movetoworkspace,~a~%"
                           workspace-number
                           (if (zero? workspace-number)
                               10
                               workspace-number)))
                 (iota 10)))

     "bind=ALT,Tab,bringactivetotop\n"
     "bind=ALT,Tab,cyclenext\n"
     "bind=SUPER,F,fullscreen\n"
     "bind=SUPER,P,pseudo\n"
     "bind=SUPER,V,togglefloating\n"
     "bind=SUPER,J,togglesplit\n"
     "bind=SUPER SHIFT,M,exit\n"
     "bind=SUPER SHIFT,Q,killactive\n"

     ;; Scroll through existing workspaces with SUPER + scroll.
     "bind=SUPER,mouse_down,workspace,e-1\n"
     "bind=SUPER,mouse_up,workspace,e+1\n"

     ;; Move/resize windows with SUPER + LMB/RMB and dragging.
     "bindm=SUPER,mouse:272,movewindow\n"
     "bindm=SUPER,mouse:273,resizewindow\n"

     "monitor=,preferred,auto,auto\n"

     "bindl=,switch:on:Lid Switch,exec," hyprctl " dispatch dpms off eDP-1\n"
     "bindl=,switch:off:Lid Switch,exec," hyprctl " dispatch dpms on eDP-1\n"

     "bind=SUPER,Return,exec,emacsclient --create-frame --no-wait --alternate-editor=''\n"
     "bind=SUPER,space,exec," %hyprland-switch-keyboard-layout "\n"
     "bind=SUPER,T,exec," alacritty "\n"
     "bind=SUPER,R,exec," rofi " -modes combi -show combi -matching fuzzy\n"
     "bind=SUPER,L,exec," swaylock " " lock-args "\n"

     "bind=,Print,exec," grimblast " copysave output " screenshot "\n"
     "bind=SUPER,Print,exec," grimblast " copysave area " screenshot "\n"

     "bindl =,XF86AudioMicMute,     exec," wpctl " set-mute   @DEFAULT_AUDIO_SOURCE@ toggle\n"
     "bindl =,XF86AudioMute,        exec," wpctl " set-mute   @DEFAULT_AUDIO_SINK@   toggle\n"
     "bindle=,XF86AudioLowerVolume, exec," wpctl " set-volume @DEFAULT_AUDIO_SINK@   5%-\n"
     "bindle=,XF86AudioRaiseVolume, exec," wpctl " set-volume @DEFAULT_AUDIO_SINK@   5%+ --limit 1.0\n"
     "bindle=,XF86MonBrightnessDown,exec," light " -U 5\n"
     "bindle=,XF86MonBrightnessUp,  exec," light " -A 5\n"

     "exec-once=" mako "\n"
     "exec-once=" xdp-gtk " -r\n"
     "exec-once=" xdp-hypr "\n"
     "exec-once=sleep 5; exec " xdp " -r\n"
     "exec-once=" wlsunset " -o eDP-1 " %dorphine-wlsunset-args "\n"
     "exec-once=" swaybg " --image " wallpaper " --mode fill --output '*'\n")))

(define %config-wget
  (plain-file
   "wgetrc"
   "hsts-file = ~/.cache/wget-hsts\n"))

(define %bashrc-direnv
  (plain-file
   "bashrc-direnv"
   "
eval \"$(direnv hook bash)\""))

(define %bashrc-eat
  (plain-file
   "bashrc-eat"
   "
[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && \\
  source \"$EAT_SHELL_INTEGRATION_DIR/bash\""))

(define %bashrc-liquidprompt
  (mixed-text-file
   "bashrc-liquidprompt"
   "
[[ $- = *i* ]] && source " liquidprompt "/share/liquidprompt/liquidprompt"))

(define %bashrc-zoxide
  (plain-file
   "bashrc-zoxide"
   "
eval \"$(zoxide init --cmd cd bash)\""))

(define %shell-profile-nvidia
  (plain-file
   "shell-profile-nvidia"
   "
if [ -d /proc/driver/nvidia ]; then
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __NV_PRIME_RENDER_OFFLOAD=1
fi"))

(define %shell-profile-wm
  (plain-file
   "shell-profile-wm"
   "
if [ -z \"${WAYLAND_DISPLAY}\" ] && [ \"${XDG_VTNR}\" -eq 1 ]; then
    exec env XDG_CURRENT_DESKTOP=Hyprland Hyprland
fi"))


;;
;; Mcron jobs
;;


(define %mcron-job-modprobed-db
  #~(job next-hour-from
         #$(file-append modprobed-db "/bin/modprobed-db storesilent")))


;;
;; Package bundles
;;


(define %home-packages
  (append (list b4
                dconf
                direnv
                firefox/dolly
                git
                `(,git "send-email")
                git-crypt
                gnupg
                hyprland
                imv
                man-pages
                mosh
                mpv/dolly
                netcat-openbsd
                openssh-sans-x
                pass-otp
                password-store
                qtwayland-5
                rofi-wayland
                rsync
                steam/dolly
                tessen
                unzip
                virt-manager
                wl-clipboard
                xdg-desktop-portal
                xdg-desktop-portal-gtk
                xdg-desktop-portal-hyprland
                xdg-utils
                zoxide
                zstd)
          (list tree-sitter-bash
                tree-sitter-c
                tree-sitter-cmake
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
          (list adwaita-icon-theme
                qogir-icon-theme)
          (list font-adobe-source-sans-pro
                font-adobe-source-serif-pro
                font-cardo
                font-chiron-hei-hk
                font-chiron-sung-hk
                font-google-noto
                font-google-noto-emoji
                font-google-noto-sans-cjk
                font-google-noto-serif-cjk
                font-lxgw-wenkai
                font-lxgw-wenkai-tc
                font-sarasa-gothic
                font-victor-mono)))


;;
;; Home environment definition for Dorphine.
;;


(home-environment
 (packages %home-packages)
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (bashrc
                   (list %bashrc-direnv
                         %bashrc-eat
                         %bashrc-liquidprompt
                         %bashrc-zoxide))))

        (service home-channels-service-type
                 (list %channel-guix
                       %channel-guixcn
                       %channel-nonguix
                       %channel-rosenthal))

        (service home-dbus-service-type)

        (service home-dotfiles-service-type
                 (home-dotfiles-configuration
                  (layout 'stow)
                  (directories '("./dotfiles"))))

        (service home-emacs-service-type
                 %dorphine-home-emacs-configuration)

        (service home-files-service-type
                 `((".guile" ,%default-dotguile)))

        (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (pinentry-program
                   (file-append pinentry-rofi/dolly "/bin/pinentry-rofi"))
                  (ssh-support? #t)))

        (service home-mcron-service-type
                 (for-home
                  (mcron-configuration
                   (jobs (list %mcron-job-modprobed-db)))))

        (service home-openssh-service-type
                 (home-openssh-configuration
                  (hosts %dorphine-ssh-hosts)))

        (service home-pipewire-service-type
                 (home-pipewire-configuration
                  (wireplumber wireplumber-minimal)))

        (service home-syncthing-service-type
                 (for-home
                  (syncthing-configuration
                   (user "hako"))))

        (service home-xdg-configuration-files-service-type
                 `(("alacritty/alacritty.toml" ,%config-alacritty)
                   ("gdb/gdbinit" ,%default-gdbinit)
                   ("hypr/hyprland.conf" ,%config-hyprland)
                   ("nano/nanorc" ,%default-nanorc)
                   ("rclone/rclone.conf" ,(testament-file-object "rclone.conf"))
                   ("wgetrc" ,%config-wget)))

        (simple-service 'setup-env-vars
                        home-environment-variables-service-type
                        `(,@%testament-xdg-base-directory-env-vars
                          ("BROWSER" . "firefox")
                          ("EDITOR" . "emacsclient")
                          ("HISTSIZE" . "10000")
                          ("VISUAL" . "$EDITOR")))

        (simple-service 'setup-shell-profile
                        home-shell-profile-service-type
                        (list %shell-profile-nvidia
                              %shell-profile-wm)))))
