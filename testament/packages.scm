(define-module (testament packages)
  #:use-module (testament counter-stop)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
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

(define-public shepherd/dolly
  (let ((base shepherd-0.10)
        (commit "9dfeb4ecd6429135f5fb8ceb6d43ae7054fbc193")
        (revision "23"))
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
                  "0v8ybj6dv625nphq05d0s3bfnm3lyypi9myvcih6caywzqy838b3"))))
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
