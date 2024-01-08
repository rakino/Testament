;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament packages)
  #:use-module (testament counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))


;;
;; Helper procedures
;;


(define rofi-dolly
  (package-input-rewriting/spec
   `(("rofi" . ,(const rofi-wayland)))))


;;
;; Inferior
;;


(define-public firefox/dolly
  (let* ((channels
          (list (channel
                 (inherit %channel-guix)
                 (commit "ab1ff7ca40b0a2d935f715dcf64f0f3128632d3d"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "71a53faf2e1925a309b480f17e5b836740ce54bc"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "121.0"))))


;;
;; Packages
;;


(define-public gopls/dolly
  (let ((base gopls))
    (package
      (inherit base)
      (propagated-inputs '())
      (inputs (package-propagated-inputs base)))))

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
