;;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system pyproject)

  #:use-module (gnu packages android)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages game-client)
  #:use-module (nonguix multiarch-container))


;;;
;;; Packages
;;;

(define-public better-adb-sync
  (package
    (name "better-adb-sync")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jb2170/better-adb-sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ri9a8r0a4i9ih0cqdj5j19dbkbqwd5m5g8ch220rh4firaj4w2"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                  ;No tests.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((adb-path (search-input-file inputs "bin/adb")))
                     (substitute* "src/BetterADBSync/argparsing.py"
                       (("'adb' ie whatever is on path")
                        (string-append "'" adb-path "'"))
                       (("\"adb\"")
                        (string-append "\"" adb-path "\"")))))))))
    (inputs (list adb))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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

(define-public steam-nvidia/dolly
  (hidden-package
   (nonguix-container->package
    (nonguix-container
     (inherit steam-nvidia-container)
     (union64
      (fhs-union
       (modify-inputs
        (@@ (nongnu packages game-client) steam-nvidia-container-libs)
        (prepend font-chiron-hei-hk
                 font-chiron-sung-hk
                 font-google-noto-emoji))
       #:name "fhs-union-64"))))))

(define-public ungoogled-chromium/dolly
  (let ((base ungoogled-chromium))
    (hidden-package
     (package
       (inherit base)
       (source #f)
       (build-system copy-build-system)
       (arguments
        (list #:phases
              #~(modify-phases %standard-phases
                  (delete 'unpack)
                  (replace 'install
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((file "chromium"))
                        (call-with-output-file file
                          (lambda (port)
                            (format port "#!~a
exec ~a --ozone-platform-hint=auto $@~%"
                                    (search-input-file inputs "bin/bash")
                                    (search-input-file inputs "bin/chromium"))))
                        (chmod file #o555)
                        (install-file file (string-append #$output "/bin"))))))))
       (inputs (list bash-minimal base))
       (propagated-inputs '())
       (native-inputs '())))))
