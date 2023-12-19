(define-module (testament packages)
  #:use-module (testament counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages texinfo)
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
                 (commit "aeb494322ca9dec4a4d66a7d063239c8536bd538"))
                (channel
                 (inherit %channel-nonguix)
                 (commit "b5b890f30a0566599a6933f3b65f56848650ec4f"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "firefox" "120.0.1"))))


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

(define-public shepherd/dolly
  (let ((base shepherd-0.10)
        (commit "5dbde1c0fbe3b19cc8e11d9733837b8ab7040e59")
        (revision "21"))
    (package
      (inherit base)
      (name "shepherd")
      (version (git-version "0.10.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/shepherd.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0yv655d2nczfrib2p9fww3y0afcigs1ghs7pqhd3bl69i4kz11l0"))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend autoconf automake gettext-minimal texinfo help2man))))))

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

(define-public wireplumber/dolly
  (let ((base wireplumber))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(cons "-Delogind=disabled" #$flags))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "elogind"))))))
