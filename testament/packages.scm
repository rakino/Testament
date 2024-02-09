;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu packages nvidia))


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

(define-public swayidle/dolly
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
