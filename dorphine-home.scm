;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-home)
  #:use-module (blobs dorphine)
  #:use-module ((testament common)
                #:select (testament-find-file
                          (testament-file-content . agathion)
                          (testament-file-object . nohitaga)))
  #:use-module (testament counter-stop)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu)
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
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zig)
  #:use-module (gnu services mcron)
  #:use-module (nongnu packages fonts)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages emacs-xyz)
  #:use-module (rosenthal packages tree-sitter)
  #:use-module (rosenthal packages web)
  #:use-module (rosenthal services child-error))


;;
;; Dolly
;;


(define rofi-dolly
  (package-input-rewriting/spec
   `(("rofi" . ,(const rofi-wayland)))))

(define buku-run-dev/dolly
  (rofi-dolly
   (package-with-patches
    buku-run-dev
    (list
     ;; (fix #27) Show Bookmark tag correctly
     (origin
       (method url-fetch)
       (uri "https://github.com/carnager/buku_run/pull/29.patch")
       (sha256
        (base32
         "0j5f6nifa3ibhgcvfn59pq7xsnbcwgj6vkcz8vm4wway2nl5b9i6")))))))

(define gopls/dolly
  (let ((base gopls))
    (package
      (inherit base)
      (propagated-inputs '())
      (inputs (package-propagated-inputs base)))))

(define pinentry-rofi/dolly
  (rofi-dolly pinentry-rofi))

(define swayidle/dolly
  (let ((base swayidle))
    (package
      (inherit base)
      (arguments
       (strip-keyword-arguments
        '(#:configure-flags)
        (package-arguments base)))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "elogind"))))))

(define wireplumber/dolly
  (let ((base wireplumber))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(cons "-Delogind=disabled" #$flags))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "elogind"))))))


;;
;; File-like
;;


(define %config-sway
  (let ((filename  "sway.conf")
        (screenshot "~/Library/Pictures/Screenshots/$(date +%Y%m%d-%H%M%S).png")
        (wallpaper  (nohitaga "112358159_p0.png"))
        (lock-args  #~(string-join
                       (list "--clock"
                             "--daemonize"
                             "--ignore-empty-password"
                             "--image"
                             #$(nohitaga "102982564_p0.jpg"))))
        (autotiling (file-append i3-autotiling "/bin/autotiling"))
        (buku_run   (file-append buku-run-dev/dolly "/bin/buku_run"))
        (grimshot   (file-append grimshot "/bin/grimshot"))
        (light      (file-append light "/bin/light"))
        (rofi       (file-append rofi-wayland "/bin/rofi"))
        (swayidle   (file-append swayidle/dolly "/bin/swayidle"))
        (swaylock   (file-append swaylock-effects "/bin/swaylock"))
        (tessen     (file-append tessen "/bin/tessen"))
        (wl-copy    (file-append wl-clipboard "/bin/wl-copy"))
        (wlsunset   (file-append wlsunset "/bin/wlsunset"))
        (wpctl      (file-append wireplumber/dolly "/bin/wpctl")))
    (mixed-text-file
     filename
     (agathion filename) "\n"

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
             '(("left"  ("h" "Left"))
               ("down"  ("t" "Down"))
               ("up"    ("n" "Up"))
               ("right" ("s" "Right")))))

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

     "bindsym $mod+e exec emacsclient --create-frame --no-wait --alternate-editor=''\n"
     "bindsym $mod+b exec " buku_run "\n"
     "bindsym $mod+d exec " tessen "\n"
     "bindsym $mod+r exec " rofi " -show combi\n"
     "bindsym $mod+l exec " swaylock " " lock-args "\n"

     "bindsym Print exec " wl-copy " --type image/png < $(" grimshot " save area " screenshot ")\n"
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
   (format #f "hsts-file = ~a/wget-hsts~%"
           (getenv "XDG_CACHE_HOME"))))

(define %shell-profile-wm
  (plain-file
   "shell-profile-wm"
   (format #f "~
if [ -z \"${WAYLAND_DISPLAY}\" ] && [ \"${XDG_VTNR}\" -eq 1 ]; then
    exec sway --unsupported-gpu
fi")))


;;
;; Emacs
;;


(define %emacs-early-init
  (list #%(setq package-enable-at-startup nil)

        #%(when (fboundp 'startup-redirect-eln-cache)
            (startup-redirect-eln-cache
             (convert-standard-filename
              (expand-file-name "eln-cache/"
                                (or (getenv "XDG_DATA_HOME")
                                    "~/.local/share")))))))

;; Copied from <https://github.com/nykma/nema/blob/develop/my-sample/font.el>
(define %emacs-extra-init
  (list #%(defvar nema-fonts '((default . "Victor Mono")
                               (unicode . "LXGW WenKai TC")
                               (modeline . "Source Serif Pro")
                               (fixed . "Sarasa Mono TC")
                               (variable . "Bembo Std"))
            "Fonts to use.")
        #%(defvar nema--font-size 12 "Font size")

        #%(defun nema//get-font-family (key)
            (alist-get key nema-fonts))

        #%(defun nema//generate-font-spec (key)
            (format "%s-%d"
                    (nema//get-font-family key)
                    nema--font-size))

        #%(defun nema//load-base-font ()
            "Load the default font for ascii characters."
            (let* ((font-spec (nema//generate-font-spec 'default)))
              (set-frame-parameter nil 'font font-spec)
              (add-to-list 'default-frame-alist (cons 'font font-spec))))

        #%(defun nema//load-face-font ()
            "Load fonts used in faces.

                This function must be called after frame creation."
            (let ((modeline-font-spec (nema//generate-font-spec 'modeline))
                  (variable-font-spec (nema//generate-font-spec 'variable))
                  (fixed-font-spec (nema//generate-font-spec 'fixed)))
              (set-face-attribute 'variable-pitch nil :font variable-font-spec :height 1.1)
              (set-face-attribute 'fixed-pitch nil :font fixed-font-spec)
              (set-face-attribute 'fixed-pitch-serif nil :font fixed-font-spec)
              (set-face-attribute 'mode-line nil :font modeline-font-spec)
              (set-face-attribute 'mode-line-inactive nil :font modeline-font-spec)
              (set-face-attribute 'tab-bar nil :font modeline-font-spec)))

        #%(defun nema//load-ext-font ()
            "Load fonts used for non-ascii characters.

                This function must be called after frame creation."
            (let ((font (frame-parameter nil 'font))
                  (font-spec (font-spec :family (nema//get-font-family 'unicode))))
              (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
                      (set-fontset-font font charset font-spec))))

        #%(defun nema/load-font()
            "Load all font configuration."
            (interactive)
            (when (display-graphic-p)
              (nema//load-base-font)
              (nema//load-ext-font)
              (nema//load-face-font)))

        #%(if (daemonp)
              (add-hook 'server-after-make-frame-hook #'nema/load-font)
              ;; Else: not in daemon
              (add-hook 'after-init-hook #'nema/load-font))))

(define %emacs-set-org-capture-template
  (list #%(defun org-hugo-new-subtree-post-capture-template ()
            "Returns `org-capture' template string for new Hugo post.
              See `org-capture-templates' for more information."
            (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
                   (slug (read-from-minibuffer "Post Slug: "))
                   (fname (org-hugo-slug slug)))
              (mapconcat #'identity
                         `(
                           ,(concat "* TODO " title)
                           ":PROPERTIES:"
                           ":EXPORT_FILE_NAME: index"
                           ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                           ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :image cover"
                           ":END:"
                           "%?\n")                ;Place the cursor here finally
                         "\n")))
        #%(setq org-capture-templates
                '(("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   (id "f419308f-3356-4379-a098-48b7f7f9d6ea")
                   (function org-hugo-new-subtree-post-capture-template)))
                org-capture-bookmark nil)))


;;
;; Mcron jobs
;;


(define %mcron-job-modprobed-db
  #~(job next-hour-from
         #$(file-append modprobed-db "/bin/modprobed-db storesilent")))


;;
;; Inferior
;;


(define firefox
  (let* ((channels
          (list (channel
                 (inherit %channel-guix)
                 (commit "1566e00fbc87ba7b6320be7f587e2e5e5c082b38"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "de0125a78318531cc1b55bd7428698e0b342e912"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "120.0"))))


;;
;; Package bundles
;;


(define %language-packages-for-emacs
  (list ccls
        gcc-toolchain-13
        go-1.20
        gopls/dolly
        python
        python-black
        rust-analyzer
        shellcheck
        zig))

(define %tree-sitter-packages-for-emacs
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
        tree-sitter-yaml))

(define %home-packages
  (append (list buku
                firefox
                git
                `(,git "send-email")
                git-crypt
                gnupg
                man-pages
                mosh
                openssh-sans-x
                pass-otp
                password-store
                qtwayland-5
                rofi-wayland
                rsync
                sway
                wl-clipboard
                xdg-desktop-portal
                xdg-desktop-portal-wlr
                xdg-utils
                zoxide)
          (list adwaita-icon-theme
                hicolor-icon-theme
                qogir-icon-theme)
          (list font-adobe-source-sans-pro
                font-adobe-source-serif-pro
                font-apple-new-york
                font-apple-sf-arabic
                font-apple-sf-compact
                font-apple-sf-mono
                font-apple-sf-pro
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
 (packages (map normalize-package %home-packages))
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (bashrc
                   (list (plain-file
                          "bashrc-eat"
                          (format #f "
[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && \\
  source \"$EAT_SHELL_INTEGRATION_DIR/bash\"~%"))))))

        (service home-channels-service-type
                 %testament-default-channels)

        (service home-dbus-service-type)

        (service home-emacs-service-type
                 (home-emacs-configuration
                  (emacs emacs-pgtk)
                  (extra-packages
                   (list emacs-magit
                         emacs-zig-mode))
                  (package-serializer %emacs-use-package-serializer)
                  (default-init
                    (emacs-configuration
                     (early-init %emacs-early-init)
                     (variables
                      '((create-lockfiles . #f)
                        (enable-recursive-minibuffers . #t)
                        (indent-tabs-mode . #f)
                        (inhibit-startup-screen . #t)
                        (minibuffer-prompt-properties
                         . (read-only t
                            cursor-intangible-mode t
                            face minibuffer-prompt))
                        (org-startup-truncated . #f)
                        (pixel-scroll-precision-mode . #t)
                        (read-extended-command-predicate
                         . command-completion-default-include-p)
                        (require-final-newline . #f)
                        (show-paren-context-when-offscreen . overlay)
                        (show-paren-style . parenthesis)
                        (truncate-lines . #f)
                        (truncate-partial-width-windows . #f)
                        (user-full-name . "Hilton Chain")
                        (user-mail-address . "hako@ultrarare.space")
                        (warning-minimum-level . :emergency)))
                     (modes
                      '((blink-cursor-mode . #f)
                        (electric-pair-mode . #t)
                        (global-display-fill-column-indicator-mode . #t)
                        (ido-mode . both)
                        (menu-bar-mode . #t)
                        (scroll-bar-mode . #f)
                        (set-fringe-mode . 1)
                        (show-paren-mode . #t)
                        (tool-bar-mode . #f)
                        (tooltip-mode . #f)))
                     (keys '(("C-x C-b" . switch-to-buffer)))
                     (extra-init %emacs-extra-init)))
                  (configured-packages
                   (list
                    (emacs-package
                     (name 'gcmh)
                     (package emacs-gcmh)
                     (load-force? #t)
                     (extra-after-load
                      (list #%(gcmh-mode +1))))

                    (emacs-package
                     (name 'no-littering)
                     (package emacs-no-littering)
                     (load-force? #t)
                     (extra-after-load
                      (list #%(no-littering-theme-backups)))
                     (extra-init
                      (list #%(setq no-littering-etc-directory
                                    (or (getenv "XDG_CONFIG_HOME")
                                        "~/.config"))
                            #%(setq no-littering-var-directory
                                    (or (getenv "XDG_DATA_HOME")
                                        "~/.local/share")))))

                    (emacs-package
                     (name 'apheleia)
                     (package emacs-apheleia)
                     (hooks '((after-init . apheleia-global-mode))))

                    (emacs-package
                     (name 'company)
                     (package emacs-company)
                     (hooks '((prog-mode . company-mode))))

                    (emacs-package
                     (name 'ctrlf)
                     (package emacs-ctrlf)
                     (hooks '((after-init . ctrlf-mode))))

                    (emacs-package
                     (name 'debbugs)
                     (package emacs-debbugs))

                    (emacs-package
                     (name 'doom-modeline)
                     (package emacs-doom-modeline)
                     (options
                      '((doom-modeline-icon . #f)
                        (doom-modeline-height . 18)))
                     (hooks '((after-init . doom-modeline-mode))))

                    (emacs-package
                     (name 'eat)
                     (package emacs-eat)
                     (options '((eat-enable-auto-line-mode . #t))))

                    (emacs-package
                     (name 'eglot)
                     (extra-packages %language-packages-for-emacs)
                     (hooks
                      (append-map
                       (lambda (mode)
                         `((,mode . eglot-ensure)))
                       '(bash-ts-mode sh-mode
                         c-mode c-ts-mode c++-mode c++-ts-mode
                         cmake-ts-mode
                         css-mode css-ts-mode
                         go-ts-mode go-mod-ts-mode
                         html-mode
                         js-mode js-ts-mode
                         js-json-mode json-ts-mode
                         markdown-mode
                         python-mode python-ts-mode
                         rust-ts-mode
                         typescript-ts-mode
                         yaml-ts-mode))))

                    (emacs-package
                     (name 'eldoc-box)
                     (package emacs-eldoc-box)
                     (extra-packages (list emacs-markdown-mode))
                     (options '((eldoc-box-only-multi-line . #t)))
                     (hooks '((eldoc-mode . eldoc-box-hover-mode))))

                    (emacs-package
                     (name 'erc)
                     (load-force? #t)
                     (extra-after-load
                      (list #%(defun erc-up ()
                                (interactive)
                                (erc-tls :server "chat.sr.ht"
                                         :user "hako/irc.libera.chat"
                                         :password #$%erc-pass)))))

                    (emacs-package
                     (name 'emacs)
                     (options
                      `((copyright-names-regexp
                         . ,#%(format "%s <%s>"
                                      user-full-name user-mail-address))))
                     (hooks
                      '((after-save . copyright-update)
                        (before-save . delete-trailing-whitespace)
                        (minibuffer-setup . cursor-intangible-mode))))

                    (emacs-package
                     (name 'geiser)
                     (package emacs-geiser)
                     (extra-packages (list emacs-geiser-guile))
                     (options
                      '((geiser-mode-start-repl-p . #t)
                        (geiser-repl-query-on-kill-p . #f)
                        (geiser-repl-use-other-window . #f))))

                    (emacs-package
                     (name 'guix-devel)
                     (package emacs-guix)
                     (hooks '((scheme-mode . guix-devel-mode))))

                    (emacs-package
                     (name 'helpful)
                     (package emacs-helpful)
                     (keys-global
                      '(("<remap> <describe-function>" . helpful-callable)
                        ("<remap> <describe-variable>" . helpful-variable)
                        ("<remap> <describe-symbol>"   . helpful-symbol)
                        ("<remap> <describe-key>"      . helpful-key)
                        ("<remap> <describe-command>"  . helpful-command)
                        ("C-c C-d" . helpful-at-point)))
                     (keys-local
                      (list (emacs-keymap
                             (name 'help-map)
                             (keys
                              '(("F" . helpful-function)
                                ("M-f" . helpful-macro)))))))

                    (emacs-package
                     (name 'hl-todo)
                     (package emacs-hl-todo)
                     (options
                      '((hl-todo-highlight-punctuation . ":")
                        (hl-todo-keyword-faces
                         . (("TODO"       warning bold)
                            ("FIXME"      error bold)
                            ("HACK"       font-lock-constant-face bold)
                            ("REVIEW"     font-lock-keyword-face bold)
                            ("NOTE"       success bold)
                            ("DEPRECATED" font-lock-doc-face bold)
                            ("DEBUG"      error bold)))))
                     (hooks '((prog-mode . hl-todo-mode))))

                    (emacs-package
                     (name 'macrostep)
                     (package emacs-macrostep)
                     (keys-global '(("C-c e" . macrostep-expand))))

                    (emacs-package
                     (name 'macrostep-geiser)
                     (package emacs-macrostep-geiser)
                     (hooks '((geiser-mode . macrostep-geiser-setup))))

                    (emacs-package
                     (name 'message)
                     (keys-global
                      '(("C-c M-m" . message-mark-inserted-region))))

                    (emacs-package
                     (name 'modus-themes)
                     (options
                      '((modus-themes-italic-constructs . #t)
                        (modus-themes-region . (bg-only no-extend))
                        (modus-themes-syntax
                         . (faint alt-syntax green-strings yellow-comments))))
                     (extra-init
                      (list #%(load-theme 'modus-operandi t))))

                    (emacs-package
                     (name 'mwim)
                     (package emacs-mwim)
                     (keys-global
                      '(("<remap> <move-beginning-of-line>" . mwim-beginning)
                        ("<remap> <move-end-of-line>" . mwim-end))))

                    (emacs-package
                     (name 'orderless)
                     (package emacs-orderless)
                     (options
                      '((completion-category-overrides
                         . ((file (styles partial-completion))))
                        (completion-styles . (orderless basic)))))

                    (emacs-package
                     (name 'org)
                     (options
                      '((org-enforce-todo-dependencies . #t)
                        (org-highlight-sparse-tree-matches . #f)
                        (org-id-track-globally . #t)
                        (org-insert-heading-respect-content . #t)
                        (org-log-into-drawer . #t)
                        (org-special-ctrl-a/e . #t)
                        (org-special-ctrl-k . #t)
                        (org-startup-folded . content)
                        (org-todo-keywords
                         . ((sequence "TODO(t!)" "DOING(g!)"
                                      "|" "DONE(d!)")
                            (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)"
                                      "|" "FIXED(f!)")
                            (sequence "|" "CANCELED(c!)" "WONTDO(w!)")))
                        (org-todo-keywords-for-agenda
                         . ("TODO" "DOING" "REPORT" "BUG" "KNOWNCAUSE" "FIXED"
                            "CANCELED" "DONE" "WONTDO")))))

                    (emacs-package
                     (name 'org-agenda)
                     (keys-global '(("C-c a" . org-agenda))))

                    (emacs-package
                     (name 'org-capture)
                     (keys-global '(("C-c c" . org-capture)))
                     (extra-after-load %emacs-set-org-capture-template))

                    (emacs-package
                     (name 'org-modern)
                     (package emacs-org-modern)
                     (hooks
                      '((org-mode . org-modern-mode)
                        (org-agenda-finalize . org-modern-agenda))))

                    (emacs-package
                     (name 'org-rainbow-tags)
                     (package emacs-org-rainbow-tags)
                     (hooks '((org-mode . org-rainbow-tags-mode))))

                    (emacs-package
                     (name 'org-re-reveal)
                     (package emacs-org-re-reveal)
                     (load-force? #t)
                     (load-after-packages '(ox)))

                    (emacs-package
                     (name 'ox-hugo)
                     (package emacs-ox-hugo)
                     (load-force? #t)
                     (load-after-packages '(ox)))

                    (emacs-package
                     (name 'pass)
                     (package emacs-pass)
                     (extra-packages (list emacs-password-store-otp))
                     (autoloads-interactive '(pass))
                     (extra-init
                      (list #%(setq pass-username-field "user"))))

                    (emacs-package
                     (name 'puni)
                     (package emacs-puni)
                     (keys-local
                      (list (emacs-keymap
                             (name 'puni-mode-map)
                             (keys '(("C-M-r" . puni-raise)
                                     ("C-M-." . puni-slurp-forward)
                                     ("C-M-," . puni-slurp-backward)
                                     ("s-." . puni-barf-forward)
                                     ("s-," . puni-barf-backward))))))
                     (hooks
                      '((after-init . puni-global-mode)
                        (minibuffer-mode . puni-disable-puni-mode))))

                    (emacs-package
                     (name 'rainbow-delimiters)
                     (package emacs-rainbow-delimiters)
                     (hooks '((prog-mode . rainbow-delimiters-mode))))

                    (emacs-package
                     (name 'rime)
                     (package emacs-rime)
                     (options
                      '((default-input-method . "rime")
                        (rime-disable-predicates
                         . (rime-predicate-after-alphabet-char-p
                            rime-predicate-prog-in-code-p))
                        (rime-show-candidate . posframe)
                        (rime-show-preedit . #t)
                        (rime-posframe-properties
                         . (:internal-border-width 1))
                        (rime-posframe-style . simple))))

                    (emacs-package
                     (name 'savehist)
                     (hooks '((after-init . savehist-mode))))

                    (emacs-package
                     (name 'treesit)
                     (extra-packages %tree-sitter-packages-for-emacs)
                     (options
                      '((major-mode-remap-alist
                         . ((sh-mode . bash-ts-mode)
                            (c-mode . c-ts-mode)
                            (c++-mode . c++-ts-mode)
                            (c-or-c++-mode . c-or-c++-ts-mode)
                            (csharp-mode . csharp-ts-mode)
                            (css-mode . css-ts-mode)
                            (java-mode . java-ts-mode)
                            (js-mode . js-ts-mode)
                            (js-json-mode . json-ts-mode)
                            (python-mode . python-ts-mode)
                            (ruby-mode . ruby-ts-mode)
                            ;; TODO: package grammar
                            ;; (conf-toml-mode . toml-ts-mode)
                            ))))
                     (auto-modes
                      ;; Copied from source file of each mode.
                      '(("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
                        ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                         . dockerfile-ts-mode)
                        ("/go\\.mod\\'" . go-mod-ts-mode)
                        ("\\.go\\'" . go-ts-mode)
                        ("\\.rs\\'" . rust-ts-mode)
                        ("\\.tsx\\'" . tsx-ts-mode)
                        ("\\.ts\\'" . typescript-ts-mode)
                        ("\\.ya?ml\\'" . yaml-ts-mode)))
                     (extra-after-load
                      (list #%(setq treesit-load-name-override-list
                                    '((c-sharp "libtree-sitter-c_sharp"
                                               "tree_sitter_c_sharp"))))))

                    (emacs-package
                     (name 'vertico)
                     (package emacs-vertico)
                     (hooks '((after-init . vertico-mode))))

                    (emacs-package
                     (name 'visual-fill-column)
                     (package emacs-visual-fill-column)
                     (options '((visual-fill-column-center-text . #t)))
                     (hooks '((org-mode . visual-fill-column-mode))))

                    (emacs-package
                     (name 'volatile-highlights)
                     (package emacs-volatile-highlights)
                     (hooks '((after-init . volatile-highlights-mode))))

                    (emacs-package
                     (name 'wakatime-mode)
                     (package emacs-wakatime-mode)
                     (options
                      `((wakatime-cli-path
                         . ,(file-append wakatime-cli-bin "/bin/wakatime-cli"))))
                     (hooks '((after-init . global-wakatime-mode))))

                    (emacs-package
                     (name 'which-key)
                     (package emacs-which-key)
                     (hooks '((after-init . which-key-mode)))
                     (extra-after-load
                      (list #%(which-key-setup-side-window-right-bottom))))

                    (emacs-package
                     (name 'wl)
                     (package emacs-wanderlust)
                     (extra-packages (list emacs-spamfilter-el))
                     (autoloads-interactive '(wl))
                     (options
                      `(,@%wanderlust-init-options
                        ;; https://github.com/wanderlust/wanderlust/issues/148
                        (elmo-network-session-idle-timeout . 110)
                        (elmo-spam-scheme . spamfilter)
                        (mime-header-accept-quoted-encoded-words . #t)
                        (wl-auto-refile-guess-functions
                         . (wl-refile-guess-by-rule
                            wl-refile-guess-by-spam))
                        (wl-default-spec . ".")
                        (wl-fcc-force-as-read . #t)
                        (wl-plugged . #f)
                        (wl-summary-auto-refile-skip-marks . #f)
                        (wl-summary-width . 185)))
                     (extra-after-load
                      (list #%(require 'wl-spam)))
                     (extra-init
                      ;; Copied from info `(wl) Minimal Settings'
                      (list #%(autoload 'wl-user-agent-compose "wl-draft" nil t)
                            #%(if (boundp 'mail-user-agent)
                                  (setq mail-user-agent 'wl-user-agent))
                            #%(if (fboundp 'define-mail-user-agent)
                                  (define-mail-user-agent
                                    'wl-user-agent
                                    'wl-user-agent-compose
                                    'wl-draft-send
                                    'wl-draft-kill
                                    'mail-send-hook)))))

                    (emacs-package
                     (name 'yasnippet)
                     (package emacs-yasnippet)
                     (hooks '((after-init . yas-global-mode))))))))

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
                  (hosts
                   (cons
                    (openssh-host
                     (match-criteria
                      "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\""))
                    %dorphine-ssh-hosts))))

        (service home-pipewire-service-type
                 (home-pipewire-configuration
                  (wireplumber wireplumber/dolly)))

        (service home-syncthing-service-type
                 (for-home
                  (syncthing-configuration
                   (user "hako"))))

        (service home-wakapi-service-type
                 (home-wakapi-configuration
                  (config %dorphine-wakapi-config)))

        (simple-service 'setup-env-vars
                        home-environment-variables-service-type
                        `(,@%xdg-base-directory-environment-variables
                          ("BROWSER" . "firefox")
                          ("EDITOR" . "emacsclient")
                          ("GUILE_AUTO_COMPILE" . "0")
                          ("MOZ_ENABLE_WAYLAND" . "1")
                          ("VISUAL" . "$EDITOR")))

        (simple-service 'setup-non-xdg-home
                        home-files-service-type
                        `((".icons/default/index.theme" ,(nohitaga "icons.theme"))))

        (simple-service 'setup-xdg-config-home
                        home-xdg-configuration-files-service-type
                        `(("git/config" ,(nohitaga "git.conf"))
                          ("gtk-3.0/settings.ini" ,(nohitaga "gtk-3.0.ini"))
                          ("hyfetch.json" ,(nohitaga "hyfetch.json"))
                          ("modprobed-db.conf" ,(nohitaga "modprobed-db.conf"))
                          ("mpv/mpv.conf" ,(nohitaga "mpv.conf"))
                          ("neofetch/config.conf" ,(nohitaga "neofetch.conf"))
                          ("npm/npmrc" ,(nohitaga "npm.conf"))
                          ("pythonstartup.py" ,(nohitaga "pythonstartup.py"))
                          ("rclone/rclone.conf" ,(nohitaga "rclone.conf"))
                          ("sway/config" ,%config-sway)
                          ("wakatime/.wakatime.cfg" ,(nohitaga "wakatime.conf"))
                          ("wanderlust/folders" ,(nohitaga "wanderlust-folders.conf"))
                          ("wgetrc" ,%config-wget)))

        (simple-service 'setup-shell-profile
                        home-shell-profile-service-type
                        (list %shell-profile-wm)))))
