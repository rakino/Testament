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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services emacs)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zig)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal packages tree-sitter)
  #:use-module (rosenthal services child-error))


;;
;; File-like
;;


(define %config-sway
  (let ((filename  "sway.conf")
        (screenshot "~/Library/Pictures/Screenshots/$(date +%Y%m%d-%H%M%S).png")
        (wallpaper  (testament-file-object "112358159_p0.png"))
        (lock-args  #~(string-join
                       (list "--clock"
                             "--daemonize"
                             "--ignore-empty-password"
                             "--image"
                             #$(testament-file-object "102982564_p0.jpg"))))
        (autotiling (file-append i3-autotiling "/bin/autotiling"))
        (grimshot   (file-append grimshot "/bin/grimshot"))
        (light      (file-append light "/bin/light"))
        (rofi       (file-append rofi-wayland "/bin/rofi"))
        (swayidle   (file-append swayidle/dolly "/bin/swayidle"))
        (swaylock   (file-append swaylock-effects "/bin/swaylock"))
        (wl-copy    (file-append wl-clipboard "/bin/wl-copy"))
        (wlsunset   (file-append wlsunset "/bin/wlsunset"))
        (wpctl      (file-append wireplumber-minimal "/bin/wpctl")))
    (mixed-text-file
     filename
     (testament-file-content filename) "\n"

     (apply string-append
            (append-map
             (match-lambda
               ((position keys)
                (map (lambda (key)
                       (format #f "~
bindsym $mod+~a focus ~a
bindsym $mod+Shift+~@*~a move ~a~%"
                               key position))
                     keys)))
             '(("left"  ("d" "Left"))
               ("down"  ("h" "Down"))
               ("up"    ("t" "Up"))
               ("right" ("n" "Right")))))

     (apply string-append
            (map (lambda (workspace-number)
                   (format #f "~
bindsym $mod+~a workspace number ~a
bindsym $mod+Shift+~@*~a move container to workspace number ~a~%"
                           workspace-number
                           (if (zero? workspace-number)
                               10
                               workspace-number)))
                 (iota 10)))

     "output eDP-1 scale 1.5\n"
     "output * bg " wallpaper " fill\n"

     "bindswitch --reload --locked lid:on output eDP-1 disable\n"
     "bindswitch --reload --locked lid:off output eDP-1 enable\n"

     "bindsym $mod+Return exec emacsclient --create-frame --no-wait --alternate-editor=''\n"
     "bindsym $mod+r exec " rofi " -show combi\n"
     "bindsym $mod+l exec " swaylock " " lock-args "\n"

     "bindsym Print exec " wl-copy " --type image/png < $(" grimshot " save output " screenshot ")\n"
     "bindsym $mod+Print exec " wl-copy " --type image/png < $(" grimshot " save window " screenshot ")\n"
     "bindsym XF86AudioRaiseVolume  exec " wpctl " set-volume @DEFAULT_AUDIO_SINK@   5%+ --limit 1.0\n"
     "bindsym XF86AudioLowerVolume  exec " wpctl " set-volume @DEFAULT_AUDIO_SINK@   5%-\n"
     "bindsym XF86AudioMute         exec " wpctl " set-mute   @DEFAULT_AUDIO_SINK@   toggle\n"
     "bindsym XF86AudioMicMute      exec " wpctl " set-mute   @DEFAULT_AUDIO_SOURCE@ toggle\n"
     "bindsym XF86MonBrightnessUp   exec " light " -A 5\n"
     "bindsym XF86MonBrightnessDown exec " light " -U 5\n"

     "exec " autotiling "\n"
     "exec " wlsunset " " %dorphine-wlsunset-args "\n"

     "exec " swayidle " -w \
timeout 300 '" swaylock " " lock-args "' \
timeout 600 'swaymsg \"output * dpms off\"' \
resume 'swaymsg \"output * dpms on\"'\n")))

(define %config-wget
  (plain-file
   "wgetrc"
   "hsts-file = ~/.cache/wget-hsts\n"))

(define %shell-profile-wm
  (plain-file
   "shell-profile-wm"
   "
if [ -z \"${WAYLAND_DISPLAY}\" ] && [ \"${XDG_VTNR}\" -eq 1 ]; then
    exec env XDG_CURRENT_DESKTOP=sway sway --unsupported-gpu
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
  (append (list buku
                firefox/dolly
                git
                `(,git "send-email")
                git-crypt
                gnupg
                imv
                man-pages
                mosh
                mpv
                openssh-sans-x
                pass-otp
                password-store
                qtwayland-5
                rofi-wayland
                rsync
                sway
                tessen
                unzip
                wl-clipboard
                xdg-desktop-portal
                xdg-desktop-portal-wlr
                xdg-utils
                zstd)
          (list ccls
                gcc-toolchain-13
                go-1.21
                gopls/dolly
                python
                python-black
                `(,rust "tools")
                shellcheck
                zig)
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
                hicolor-icon-theme
                qogir-icon-theme)
          (list font-adobe-source-sans-pro
                font-adobe-source-serif-pro
                font-cardo
                font-chiron-hei-hk
                font-chiron-sung-hk
                font-google-noto
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
                   (list (plain-file
                          "bashrc-eat"
                          "
[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && \\
  source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")))))

        (service home-channels-service-type
                 (list %channel-guix
                       %channel-guixcn
                       %channel-nonguix
                       %channel-rosenthal))

        (service home-dbus-service-type)

        (service home-emacs-service-type
                 %dorphine-home-emacs-configuration)

        (service home-files-service-type
                 `((".guile" ,%default-dotguile)
                   (".icons/default/index.theme" ,(testament-file-object "icons.theme"))))

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

        (service home-wakapi-service-type
                 (home-wakapi-configuration
                  (config %dorphine-wakapi-config)))

        (service home-xdg-configuration-files-service-type
                 `(("gdb/gdbinit" ,%default-gdbinit)
                   ("git/config" ,(testament-file-object "git.conf"))
                   ("gtk-3.0/settings.ini" ,(testament-file-object "gtk-3.0.ini"))
                   ("hyfetch.json" ,(testament-file-object "hyfetch.json"))
                   ("modprobed-db.conf" ,(testament-file-object "modprobed-db.conf"))
                   ("mpv/mpv.conf" ,(testament-file-object "mpv.conf"))
                   ("nano/nanorc" ,%default-nanorc)
                   ("neofetch/config.conf" ,(testament-file-object "neofetch.conf"))
                   ("npm/npmrc" ,(testament-file-object "npm.conf"))
                   ("pythonstartup.py" ,(testament-file-object "pythonstartup.py"))
                   ("rclone/rclone.conf" ,(testament-file-object "rclone.conf"))
                   ("sway/config" ,%config-sway)
                   ("wakatime/.wakatime.cfg" ,(testament-file-object "wakatime.conf"))
                   ("wanderlust/folders" ,(testament-file-object "wanderlust-folders.conf"))
                   ("wgetrc" ,%config-wget)))

        (simple-service 'activate-directories
                        home-activation-service-type
                        %testament-xdg-base-directory-activation)

        (simple-service 'setup-env-vars
                        home-environment-variables-service-type
                        `(,@%testament-xdg-base-directory-env-vars
                          ("BROWSER" . "firefox")
                          ("EDITOR" . "emacsclient")
                          ("HISTSIZE" . "10000")
                          ("MOZ_ENABLE_WAYLAND" . "1")
                          ("VISUAL" . "$EDITOR")))

        (simple-service 'setup-shell-profile
                        home-shell-profile-service-type
                        (list %shell-profile-wm)))))
