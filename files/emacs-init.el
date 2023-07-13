;; -*- lexical-binding: t -*-

(setq %repo-dir "~/Repository"
      %work-dir "~/Workspace")

(setq use-package-always-defer t)
(require 'use-package)

(use-package gcmh
  :init (gcmh-mode +1))

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory (getenv "XDG_CONFIG_HOME")
        no-littering-var-directory (getenv "XDG_DATA_HOME"))
  :config
  (no-littering-theme-backups)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load-file custom-file))



(use-package emacs
  :bind ("C-x C-b" . switch-to-buffer)
  :hook
  (before-save . delete-trailing-whitespace)
  (minibuffer-setup . cursor-intangible-mode)
  :init
  (setq create-lockfiles nil
        enable-recursive-minibuffers t
        inhibit-startup-screen t
        org-startup-truncated nil
        require-final-newline nil
        show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay
        truncate-partial-width-windows nil
        warning-minimum-level :emergency)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; buffer-local variables
  (setq-default indent-tabs-mode nil
                truncate-lines nil)

  (blink-cursor-mode -1)
  (electric-pair-mode +1)
  (ido-mode 'both)
  (menu-bar-mode +1)
  (scroll-bar-mode -1)
  (show-paren-mode +1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (global-display-fill-column-indicator-mode +1)

  (set-fringe-mode 1)

  (defun god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar))))



(use-package apheleia
  :init (apheleia-global-mode +1))

(use-package company
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-quick-access t
        company-idle-delay .2
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-require-match nil))

(use-package ctrlf
  :init (ctrlf-mode +1))

(use-package denote
  :hook (dired-mode . denote-dired-mode-in-directories)
  :init
  (setq denote-directory (expand-file-name "Notes" %work-dir)
        denote-dired-directories (list denote-directory)
        denote-known-keywords '()))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 18))

(use-package eglot
  :hook
  ((bash-ts-mode sh-mode                ;TODO bash-language-server
    c-mode c-ts-mode c++-mode c++-ts-mode
    cmake-ts-mode                       ;TODO cmake-language-server
    css-mode css-ts-mode                ;TODO vscode-css-language-server
    go-ts-mode go-mod-ts-mode
    html-mode                           ;TODO vscode-html-language-server
    js-mode js-ts-mode                  ;TODO typescript-language-server
    js-json-mode json-ts-mode           ;TODO vscode-json-language-server
    markdown-mode                       ;TODO vscode-markdown-language-server
    python-mode python-ts-mode          ;TODO jedi-language-server
    rust-ts-mode                        ;TODO rust-analyzer
    typescript-ts-mode                  ;TODO typescript-language-server
    yaml-ts-mode                        ;TODO yaml-language-server
    ) . eglot-ensure))

(use-package eldoc-box
  :init (setq eldoc-box-only-multi-line t)
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package geiser
  :init
  (setq geiser-guile-load-path
        (list (expand-file-name "guix" %repo-dir)
              (expand-file-name "nonguix" %repo-dir)
              (expand-file-name "Rosenthal" %work-dir)
              (expand-file-name "Testament" %work-dir))
        geiser-repl-use-other-window nil
        geiser-repl-query-on-kill-p nil
        geiser-mode-start-repl-p t))

(use-package god-mode
  :hook (post-command . god-mode-update-cursor-type)
  :bind (("<escape>" . god-mode-all)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window)
         :map god-local-mode-map
         ("." . repeat)
         ("i" . god-local-mode)
         ("[" . backward-paragraph)
         ("]" . forward-paragraph)
         ("C-x C-o" . other-window)
         ("C-x C-k" . kill-buffer))
  :init (god-mode-all)
  :config (setq god-exempt-major-modes
                (cons 'pass-mode god-exempt-major-modes)))

(use-package guix-devel
  :hook (scheme-mode . guix-devel-mode))

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)
         ([remap describe-command]  . helpful-command)
         :map help-map
         ("F" . helpful-function)
         ("M-f" . helpful-macro)
         :map global-map
         ("C-c C-d" . helpful-at-point)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("DEBUG"      error bold))))

(use-package macrostep
  :bind (("C-c e" . macrostep-expand))
  :init (use-package macrostep-geiser))

(use-package modus-themes
  :no-require t
  :functions (true-color-p)
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-syntax '(faint alt-syntax green-strings yellow-comments)
        modus-themes-region '(bg-only no-extend))
  (load-theme 'modus-operandi t))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles partial-completion)))))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook (auto-save . org-save-all-org-buffers)
  :config
  (setq org-insert-heading-respect-content t
        org-startup-folded 'content
        org-enforce-todo-dependencies t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-highlight-sparse-tree-matches nil
        org-id-track-globally t
        org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "DOING(g!)" "|" "DONE(d!)")
          (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f!)")
          (sequence "|" "CANCELED(c!)" "WONTDO(w!)")))
  (setq org-todo-keywords-for-agenda
        '("TODO" "DOING" "REPORT" "BUG" "KNOWNCAUSE" "FIXED" "CANCELED" "DONE" "WONTDO")))

(use-package org-capture
  :init
  (defun org-hugo-new-subtree-post-capture-template ()
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
  :config
  (setq org-capture-templates
        '(("h"                ;`org-capture' binding + h
           "Hugo post"
           entry
           ;; It is assumed that below file is present in `org-directory'
           ;; and that it has a "Blog Ideas" heading. It can even be a
           ;; symlink pointing to the actual location of all-posts.org!
           (id "f419308f-3356-4379-a098-48b7f7f9d6ea")
           (function org-hugo-new-subtree-post-capture-template)))
        org-capture-bookmark nil))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-rainbow-tags
  :hook (org-mode . org-rainbow-tags-mode))

(use-package ox-hugo
  :demand t
  :after (ox))

(use-package pass
  :commands (pass)
  :init (setq pass-username-field "user"))

(use-package puni
  :init (puni-global-mode +1)
  :hook (minibuffer-mode . puni-disable-puni-mode)
  :bind (:map puni-mode-map
              ("C-M-r" . puni-raise)
              ("C-M-." . puni-slurp-forward)
              ("C-M-," . puni-slurp-backward)
              ("s-." . puni-barf-forward)
              ("s-," . puni-barf-backward)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rime
  :custom (default-input-method "rime")
  :init
  (setq rime-show-preedit 't
        rime-show-candidate 'posframe
        rime-posframe-style 'simple)
  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p))
  (setq rime-posframe-properties
        (list :internal-border-width 1)))

(use-package savehist
  :init (savehist-mode +1))

(use-package treesit
  :init
  (setq treesit-extra-load-path
        (list (getenv "TREE_SITTER_GRAMMAR_PATH")))

  (setq treesit-load-name-override-list
        '((c-sharp "libtree-sitter-c_sharp" "tree_sitter_c_sharp")))

  (setq major-mode-remap-alist
        '((sh-mode . bash-ts-mode)
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
          ;; (conf-toml-mode . toml-ts-mode) ;TODO: package grammar
          ))

  ;; Copied from source file of each mode.
  (let ((auto-modes
         '(("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
           ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
            . dockerfile-ts-mode)
           ("/go\\.mod\\'" . go-mod-ts-mode)
           ("\\.go\\'" . go-ts-mode)
           ("\\.rs\\'" . rust-ts-mode)
           ("\\.tsx\\'" . tsx-ts-mode)
           ("\\.ts\\'" . typescript-ts-mode)
           ("\\.ya?ml\\'" . yaml-ts-mode)
           )))
    (dolist (mode auto-modes)
      (push mode auto-mode-alist))))

(use-package vertico
  :init (vertico-mode +1))

(use-package visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package volatile-highlights
  :init (volatile-highlights-mode +1))

(use-package wakatime-mode
  :init (global-wakatime-mode +1))

(use-package which-key
  :init
  (which-key-mode +1)
  (which-key-setup-side-window-right-bottom))

(use-package wl
  :commands (wl)
  :init
  ;; Copied from info `(wl) Minimal Settings'
  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))
  :config
  (use-package message
    :bind (:map wl-draft-mode-map
                ("C-c M-m" . message-mark-inserted-region))))

(use-package yasnippet
  :hook (after-init . yas-global-mode))


;; Copied from <https://github.com/nykma/nema/blob/develop/my-sample/font.el>
(defvar nema-fonts '((default . "Victor Mono")
                     (unicode . "LXGW WenKai TC")
                     (modeline . "Source Serif Pro")
                     (fixed . "Sarasa Mono SC")
                     (variable . "Bembo Std"))
  "Fonts to use.")
(defvar nema--font-size 12 "Font size")

(defun nema//get-font-family (key)
  (alist-get key nema-fonts))

(defun nema//generate-font-spec (key)
  (format "%s-%d"
          (nema//get-font-family key)
          nema--font-size))

(defun nema//load-base-font ()
  "Load the default font for ascii characters."
  (let* ((font-spec (nema//generate-font-spec 'default)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist (cons 'font font-spec))))

(defun nema//load-face-font ()
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

(defun nema//load-ext-font ()
  "Load fonts used for non-ascii characters.

                This function must be called after frame creation."
  (let ((font (frame-parameter nil 'font))
        (font-spec (font-spec :family (nema//get-font-family 'unicode))))
    (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
      (set-fontset-font font charset font-spec))))

(defun nema/load-font()
  "Load all font configuration."
  (interactive)
  (when (display-graphic-p)
    (nema//load-base-font)
    (nema//load-ext-font)
    (nema//load-face-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'nema/load-font)
  ;; Else: not in daemon
  (add-hook 'after-init-hook #'nema/load-font))
