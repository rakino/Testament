;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-emacs)
  #:use-module (blobs dorphine)
  #:use-module (testament common)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu home services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages shells)
  #:export (%dorphine-home-emacs-configuration))

(define %emacs-early-init
  (list #%(setq package-enable-at-startup nil)

        #%(when (fboundp 'startup-redirect-eln-cache)
            (startup-redirect-eln-cache "~/.local/share/eln-cache"))))

(define %emacs-set-org-capture-template
  (list #%(defun org-hugo-new-subtree-post-capture-template ()
            "Returns `org-capture' template string for new Hugo post.
              See `org-capture-templates' for more information."
            ;; Prompt to enter the post title
            (let* ((title (read-from-minibuffer "Post Title: "))
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
                           "%?\n")      ;Place the cursor here finally
                         "\n")))
        #%(setq org-capture-templates
                '(("h"                  ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   (id "f419308f-3356-4379-a098-48b7f7f9d6ea")
                   (function org-hugo-new-subtree-post-capture-template)))
                org-capture-bookmark nil)))

(define %dorphine-home-emacs-configuration
  (home-emacs-configuration
   (emacs emacs-pgtk)
   (extra-packages
    (list emacs-zig-mode))
   (package-serializer %emacs-use-package-serializer)
   (default-init
     (emacs-configuration
      (early-init %emacs-early-init)
      (extra-init-files
       `(("font.el" . ,(testament-file-object "emacs-init-font.el"))))
      (variables
       '((create-lockfiles . #f)
         (enable-recursive-minibuffers . #t)
         (fill-column . 80)
         (find-function-C-source-directory . "~/Repository/emacs/src")
         (indent-tabs-mode . #f)
         (inhibit-startup-screen . #t)
         (minibuffer-prompt-properties
          . (read-only t cursor-intangible-mode t face minibuffer-prompt))
         (org-startup-truncated . #f)
         (pixel-scroll-precision-mode . #t)
         (read-extended-command-predicate
          . command-completion-default-include-p)
         (require-final-newline . #f)
         (show-paren-context-when-offscreen . overlay)
         (show-paren-style . parenthesis)
         (user-full-name . "Hilton Chain")
         (user-mail-address . "hako@ultrarare.space")
         (warning-minimum-level . :emergency)
         (word-wrap-by-category . #t)))
      (modes
       '((blink-cursor-mode . #f)
         (electric-pair-mode . #t)
         (ido-mode . both)
         (menu-bar-mode . #t)
         (scroll-bar-mode . #f)
         (set-fringe-mode . 1)
         (show-paren-mode . #t)
         (tool-bar-mode . #f)
         (tooltip-mode . #f)))
      (keys '(("C-x C-b" . switch-to-buffer)))))
   (configured-packages
    (list
     (emacs-package
      (name 'no-littering)
      (package emacs-no-littering)
      (load-force? #t)
      (extra-after-load
       (list #%(no-littering-theme-backups)))
      (extra-init
       (list #%(setq no-littering-etc-directory "~/.config")
             #%(setq no-littering-var-directory "~/.local/share"))))

     (emacs-package
      (name 'emacs)
      (hooks
       '((before-save . delete-trailing-whitespace)
         (minibuffer-setup . cursor-intangible-mode)
         (org-mode . visual-line-mode)
         (prog-mode . display-fill-column-indicator-mode))))

     (emacs-package
      (name 'adaptive-wrap)
      (package emacs-adaptive-wrap)
      (hooks '((visual-line-mode . adaptive-wrap-prefix-mode))))

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
      (options '((eat-enable-auto-line-mode . #t)))
      (hooks '((eshell-mode . eat-eshell-mode))))

     (emacs-package
      (name 'eglot)
      (hooks
       (map (cut cons <> 'eglot-ensure)
            '(cmake-ts-mode
              bash-ts-mode sh-mode
              c-mode c-ts-mode c++-mode c++-ts-mode
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
      (name 'envrc)
      (package emacs-envrc)
      (hooks '((after-init . envrc-global-mode))))

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
      (name 'esh-autosuggest)
      (package emacs-esh-autosuggest)
      (hooks '((eshell-mode . esh-autosuggest-mode))))

     (emacs-package
      (name 'eshell)
      (options
       '((eshell-history-size . #f)
         (eshell-visual-commands . #f)))
      (extra-after-load
       (list #%(dolist
                (module '(eshell-elecslash eshell-xtra))
                (add-to-list 'eshell-modules-list module t)))))

     (emacs-package
      (name 'eshell-syntax-highlighting)
      (package emacs-eshell-syntax-highlighting)
      (hooks '((after-init . eshell-syntax-highlighting-global-mode))))

     (emacs-package
      (name 'fish-completion)
      (package emacs-fish-completion)
      (extra-packages (list emacs-bash-completion))
      (options
       `((fish-completion-command . ,(file-append fish "/bin/fish"))
         (fish-completion-fallback-on-bash-p . #t)))
      (hooks '((after-init . global-fish-completion-mode))))

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

     ;; https://gitlab.com/emacs-guix/emacs-guix/-/issues/19
     ;; See also: `guix-repl-guile-args'
     (emacs-package
      (name 'guix-repl)
      (package emacs-guix)
      (options
       '((guix-guile-program . ("guix" "repl"))
         (guix-repl-use-latest . #f)
         (guix-repl-use-server . #f)))
      (extra-after-load
       (list #%(setq guix-config-scheme-compiled-directory nil)
             #%(setq guix-config-guix-scheme-directory nil)
             #%(setq guix-load-path nil))))

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
      (options '((hl-todo-highlight-punctuation . ":")))
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
      (name 'magit)
      (package emacs-magit)
      (options '((magit-define-global-key-bindings . recommended))))

     (emacs-package
      (name 'magit-extras)
      (package emacs-magit)
      (load-force? #t)
      (load-after-packages '(project)))

     (emacs-package
      (name 'mbsync)
      (package emacs-mbsync)
      (options
       `((mbsync-executable . ,(file-append isync "/bin/mbsync"))
         (mbsync-args
          . ("--config" ,(testament-file-object "mbsync.conf")
             "-a")))))

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
       '((completion-category-overrides . ((file (styles partial-completion))))
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
          . ((sequence "TODO(t!)" "DOING(g!)" "|" "DONE(d!)")
             (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f!)")
             (sequence "|" "CANCELED(c!)" "WONTDO(w!)")))
         (org-todo-keywords-for-agenda
          . ("TODO" "DOING" "DONE"
             "REPORT" "BUG" "KNOWNCAUSE" "FIXED"
             "CANCELED" "WONTDO")))))

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
      (hooks '((after-init . puni-global-mode))))

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
      (hooks '((visual-line-mode . visual-fill-column-mode))))

     (emacs-package
      (name 'volatile-highlights)
      (package emacs-volatile-highlights)
      (hooks '((after-init . volatile-highlights-mode))))

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
       '((elmo-localdir-folder-path . "~/.local/share/mail")
         (elmo-maildir-folder-path . "~/.local/share/mail")
         (elmo-msgdb-extra-fields . ("list-id"))
         (elmo-spam-scheme . spamfilter)
         (mime-edit-pgp-signers . ("F4C2D1DF3FDEEA63D1D30776ACC66D09CA528292"))
         (mime-header-accept-quoted-encoded-words . #t)
         (wl-auto-refile-guess-functions
          . (wl-refile-guess-by-rule
             wl-refile-guess-by-spam))
         (wl-default-folder . ".inbox")
         (wl-default-spec . ".")
         (wl-draft-always-delete-myself . #t)
         (wl-fcc . ".inbox")
         (wl-fcc-force-as-read . #t)
         (wl-local-domain . "ultrarare.space")
         (wl-plugged . #f)
         (wl-quicksearch-folder . "[]~/.local/share/mail!rgrep")
         (wl-refile-rule-alist
          . (("List-Id"
              ("bug-guix"     . ".hako@ultrarare.space/Zbug-guix")
              ("guix-devel"   . ".hako@ultrarare.space/Zguix-devel")
              ("guix-patches" . ".hako@ultrarare.space/Zguix-patches")
              ("help-guix"    . ".hako@ultrarare.space/Zhelp-guix")
              ("info-guix"    . ".hako@ultrarare.space/Zinfo-guix"))))
         (wl-smtp-authenticate-type . "plain")
         (wl-smtp-connection-type . ssl)
         (wl-smtp-posting-port . 465)
         (wl-smtp-posting-server . "mail.boiledscript.com")
         (wl-smtp-posting-user . "hako@ultrarare.space")
         (wl-summary-auto-refile-skip-marks . #f)
         (wl-summary-width . 150)))
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
                     'mail-send-hook)))))))))
