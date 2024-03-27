;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module (guix packages)
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

(define-public steam-nvidia-container/dolly
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
     #:name "fhs-union-64"))))

(define-public steam-nvidia/dolly
  (nonguix-container->package steam-nvidia-container/dolly))
