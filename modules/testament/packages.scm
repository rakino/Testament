;;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system emacs)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages video))


;;;
;;; Packages
;;;

(define-public emacs-eat/dolly
  (hidden-package
   (package-with-extra-patches emacs-eat
     (list (origin
             (method url-fetch)
             (uri "https://codeberg.org/akib/emacs-eat/pulls/133.patch")
             (sha256
              (base32
               "1vsq76k1gbaabiwqnx47ksq5s5pfhdvhmwxxywvjc5y9gmp3yglz")))))))

(define-public emacs-eglot-booster/dolly
  (hidden-package
   (package
     (inherit emacs-eglot-booster)
     (arguments
      (list #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'fix-command-reference
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "eglot-booster.el"
                      (("(['d].\")(emacs-lsp-booster)" _ prefix command)
                       (string-append
                        prefix
                        (search-input-file
                         inputs (string-append "bin/" command))))))))))
     (inputs (list emacs-lsp-booster)))))

(define-public emacs-isearch-mb
  (package
    (name "emacs-isearch-mb")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/isearch-mb-"
                           version ".tar"))
       (sha256
        (base32 "1b4929vr5gib406p51zcvq1ysmzvnz6bs1lqwjp517kzp6r4gc5y"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/astoff/isearch-mb")
    (synopsis "Control isearch from the minibuffer")
    (description "")
    (license license:gpl3+)))

(define-public emacs-nftables-mode
  (package
    (name "emacs-nftables-mode")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/nftables-mode-"
                           version ".tar"))
       (sha256
        (base32 "1wjw6n60kj84j8gj62mr6s97xd0aqvr4v7npyxwmhckw9z13xcqv"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/nftables-mode.html")
    (synopsis "Major mode for editing nftables scripts")
    (description
     "@code{nftables-mode} is an Emacs major mode for editing nftables scripts.
It currently only offers basic highlighting and primitive indentation.")
    (license license:gpl3+)))

(define-public emacs-treesit-auto
  (package
    (name "emacs-treesit-auto")
    ;; NOTE: Not tagged, also change commit when updating.
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/renzmann/treesit-auto")
             (commit "016bd286a1ba4628f833a626f8b9d497882ecdf3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03bvam7cpxqp4idhd235n76qdqhsbgw7m2lphy8qqwslbmcq23m4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/renzmann/treesit-auto")
    (synopsis "Automatically use tree-sitter major modes")
    (description
     "@code{treesit-auto} is an Emacs package for automatically using tree-sitter
major modes and falling back to the original major mode when its tree-sitter
counterpart is unavailable.")
    (license license:gpl3+)))

(define-public mpv/dolly
  (package
    (inherit mpv)
    (propagated-inputs '())
    (inputs
     (append
      (package-propagated-inputs mpv)
      (package-inputs mpv)))))
