;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu packages nvidia)
  #:use-module (nonguix multiarch-container))


;;
;; Helper procedures
;;


(define rofi-dolly
  (package-input-rewriting/spec
   `(("rofi" . ,(const rofi-wayland)))))


;;
;; Packages
;;


(define-public firefox/dolly
  (replace-mesa firefox))

(define-public mpv/dolly
  (replace-mesa mpv))

(define-public pinentry-rofi/dolly
  (rofi-dolly pinentry-rofi))

(define steam-nvidia/dolly
  (nonguix-container->package
   (nonguix-container
    (inherit steam-nvidia-container)
    (union64
     (fhs-union
      (modify-inputs
          (@@ (nongnu packages game-client) steam-nvidia-container-libs)
        (prepend font-google-noto
                 font-google-noto-emoji
                 font-google-noto-sans-cjk
                 font-google-noto-serif-cjk))
      #:name "fhs-union-64")))))

(define-public steam/dolly
  (package
    (inherit steam-nvidia/dolly)
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'unpack)
               (replace 'install
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((file "steam"))
                     (call-with-output-file file
                       (lambda (port)
                         (format port "#!~a
GDK_SCALE=1.5 LANG=zh_CN.UTF-8 TZ=Asia/Hong_Kong exec ~a $@~%"
                                 (search-input-file inputs "bin/bash")
                                 (search-input-file inputs "bin/steam"))))
                     (chmod file #o555)
                     (install-file file (string-append #$output "/bin"))))))))
    (inputs (list bash-minimal steam-nvidia/dolly))
    (propagated-inputs '())
    (native-inputs '())))
