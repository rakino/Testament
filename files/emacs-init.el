;; -*- lexical-binding: t -*-

(setq use-package-always-defer t)
(require 'use-package)

(use-package gcmh
  :init (gcmh-mode +1))

(use-package no-littering
  :demand t
  :hook
  (before-save . delete-trailing-whitespace)
  (minibuffer-setup . cursor-intangible-mode)
  :init
  (setq no-littering-etc-directory (or (getenv "XDG_CONFIG_HOME")
                                       "~/.config")
        no-littering-var-directory (or (getenv "XDG_DATA_HOME")
                                       "~/.local/share"))
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file
        (no-littering-expand-var-file-name "custom.el"))

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load-file custom-file))



(use-package straight
  :demand t
  :init
  (setq straight-base-dir no-littering-var-directory
        straight-repository-branch "develop")
  :config
  (straight-use-package-mode +1)
  (mapc 'straight-use-recipes
        '((org-elpa :local-repo nil)
          (melpa
           :type git :host github :build nil
           :repo "melpa/melpa")
          (gnu-elpa-mirror
           :type git :host github :build nil
           :repo "emacs-straight/gnu-elpa-mirror")
          (nongnu-elpa
           :type git :build nil :local-repo "nongnu-elpa"
           :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git")
          (el-get
           :type git :host github :build nil
           :repo "dimitri/el-get")
          (emacsmirror-mirror
           :type git :host github :build nil
           :repo "emacs-straight/emacsmirror-mirror"))))

(use-package straight-x
  :commands (straight-x-fetch-all))



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
(setq-default fill-column 100
              indent-tabs-mode nil
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

(bind-key "C-x C-b" #'switch-to-buffer)

(defun god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))



(use-package ace-link
  :init (ace-link-setup-default)
  :bind ("C-c M-o" . ace-link-org))

(use-package ace-jump-mode
  :init (eval-after-load "ace-jump-mode"
          '(ace-jump-mode-enable-mark-sync))
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package apheleia
  :init (apheleia-global-mode +1))

(use-package browse-kill-ring
  :bind ("C-c y" . browse-kill-ring))

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

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 18))

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode +1))

(use-package geiser
  :init
  (setq geiser-repl-use-other-window nil
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

(use-package helpful
  :straight t
  :bind (([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-symbol]   . #'helpful-symbol)
         ([remap describe-key]      . #'helpful-key)
         :map help-map
         ("F" . #'helpful-function)
         ("M-f" . #'helpful-macro)
         ("C" . #'helpful-command)
         :map global-map
         ("C-c C-d" . #'helpful-at-point))
  :config
  (if (version<= "29" emacs-version)
      ;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
      (defvar read-symbol-positions-list nil)))

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
  :bind (([remap move-beginning-of-line] . #'mwim-beginning)
         ([remap move-end-of-line] . #'mwim-end)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles partial-completion)))))

(use-package org
  :bind (("C-c a" . #'org-agenda)
         ("C-c c" . #'org-capture))
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

(use-package vertico
  :init (vertico-mode +1))

(use-package visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package volatile-highlights
  :init (volatile-highlights-mode +1))

(use-package vundo
  :bind (("C-x u" . #'vundo)
         :map god-local-mode-map
         ("C-x C-u" . #'vundo)))

(use-package wakatime-mode
  :init
  (setq wakatime-cli-path "/bin/wakatime-cli")
  (global-wakatime-mode +1))

(use-package which-key
  :init
  (which-key-mode +1)
  (which-key-setup-side-window-right-bottom))

(use-package windmove
  :bind (("s-<left>" . windmove-left)
         ("s-<right>" . windmove-right)
         ("s-<up>" . windmove-up)
         ("s-<down>" . windmove-down)))

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
        'mail-send-hook)))

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
