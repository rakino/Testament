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
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages admin)
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
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
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
  #:use-module (gnu services mcron)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages emacs-xyz)
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

(define hyprland/dolly
  (let ((base hyprland))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags #~'())
          #~(append #$flags '("-Dxwayland=disabled"))))))))

(define pinentry-rofi/dolly
  (rofi-dolly pinentry-rofi))


;;
;; File-like
;;


(define %config-hyprland
  (let ((filename   "hyprland.conf")
        (wallpaper (nohitaga "94280741_p0.jpg"))
        (lockpaper (nohitaga "102982564_p0.jpg"))
        (alacritty (file-append alacritty "/bin/alacritty"))
        (amixer    (file-append alsa-utils "/bin/amixer"))
        (buku_run  (file-append buku-run-dev/dolly "/bin/buku_run"))
        (hyprctl   (file-append hyprland/dolly "/bin/hyprctl"))
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

     (apply
      string-append
      (append-map
       (lambda (fmt)
         (map (lambda (n)
                (format #f fmt n (if (zero? n)
                                     10
                                     n)))
              (iota 10)))
       '(;; Switch workspaces with mainMod + [0-9]
         "bind = $mainMod, ~a, workspace, ~a~%"
         ;; Move active window to a workspace with mainMod + SHIFT + [0-9]
         "bind = $mainMod SHIFT, ~a, movetoworkspace, ~a~%")))

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

     "bindle = , XF86AudioRaiseVolume, exec, " amixer " -q --card 1 set Master 5%+\n"
     "bindle = , XF86AudioLowerVolume, exec, " amixer " -q --card 1 set Master 5%-\n"
     "bindle = , XF86AudioMute, exec, " amixer " -q --card 1 set Master toggle\n"
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
                 (commit "4e531e55dcdc99c83bcfe3eec67c3fd95c7b6ca7"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "bce51ba1931310e5cde22c26b290a534ff7120bd"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "117.0"))))


;;
;; Package bundles
;;


(define %language-packages-for-emacs
  (list ccls
        gcc-toolchain-12
        go-1.20
        gopls/dolly
        shellcheck))

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
                exa
                firefox
                git
                `(,git "send-email")
                git-crypt
                gnupg
                grimblast
                hyprland/dolly
                man-pages
                mosh
                openssh-sans-x
                pass-otp
                password-store
                python-prompt-toolkit
                qtwayland-5
                rofi-wayland
                rsync
                wl-clipboard
                xdg-desktop-portal
                xdg-desktop-portal-wlr
                xdg-utils
                xonsh
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
                font-victor-mono)))


;;
;; Home environment definition for Dorphine.
;;


(home-environment
 (packages (map normalize-package %home-packages))
 (services
  (list (service home-channels-service-type
                 %testament-default-channels)

        (service home-dbus-service-type)

        (service home-emacs-service-type
                 (home-emacs-configuration
                  (emacs emacs-pgtk)
                  (extra-packages
                   (list emacs-magit
                         emacs-xonsh-mode))
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
                        (read-extended-command-predicate
                         . command-completion-default-include-p)
                        (require-final-newline . #f)
                        (show-paren-context-when-offscreen . overlay)
                        (show-paren-style . parenthesis)
                        (truncate-lines . #f)
                        (truncate-partial-width-windows . #f)
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
                     (name 'doom-modeline)
                     (package emacs-doom-modeline)
                     (options
                      '((doom-modeline-icon . #f)
                        (doom-modeline-height . 18)))
                     (hooks '((after-init . doom-modeline-mode))))

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
                     (options
                      `((erc-autojoin-channels-alist . ,%erc-channels)
                        (erc-sasl-user . "hako")
                        (erc-sasl-password . ,%erc-pass)))
                     (extra-after-load
                      (list #%(setq erc-modules (cons 'sasl erc-modules))
                            #%(defun erc-up ()
                                (interactive)
                                (erc-tls :server erc-default-server
                                         :port erc-default-port-tls
                                         :nick "hako")))))

                    (emacs-package
                     (name 'emacs)
                     (hooks
                      '((before-save . delete-trailing-whitespace)
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
                     (hooks '((text-mode . visual-fill-column-mode))))

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
                          ("git/config" ,(nohitaga "git.conf"))
                          ("gtk-3.0/settings.ini" ,(nohitaga "gtk-3.0.ini"))
                          ("hypr/hyprland.conf" ,%config-hyprland)
                          ("mpv/mpv.conf" ,(nohitaga "mpv.conf"))
                          ("npm/npmrc" ,(nohitaga "npm.conf"))
                          ("pythonstartup.py" ,(nohitaga "pythonstartup.py"))
                          ("wanderlust/folders" ,(nohitaga "wanderlust-folders.conf"))
                          ("wgetrc" ,%config-wget)
                          ("xonsh/rc.xsh" ,(nohitaga "xonsh.xsh"))))

        (simple-service 'setup-shell-profile
                        home-shell-profile-service-type
                        (list %shell-profile-wm)))))
