;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (paradise-system)
  #:use-module (common)
  #:use-module (counter-stop)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (rosenthal packages linux)
  #:use-module (rosenthal services child-error))

(load (summon "blob-paradise-system.scm"))


;;
;; Operating system definition for Paradise.
;;


(define %paradise-initrd-modules
  (append '("virtio_scsi")
          %rosenthal-base-initrd-modules
          %base-initrd-modules))

(operating-system
  (kernel linux-hardened)

  (kernel-arguments %rosenthal-default-kernel-arguments)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (initrd (lambda (file-systems . rest)
            (raw-initrd file-systems
                        #:linux kernel
                        #:linux-modules %paradise-initrd-modules
                        #:helper-packages (list btrfs-progs/static))))

  (initrd-modules %paradise-initrd-modules)

  (firmware '())

  (host-name "paradise")

  (file-systems
   (let ((rootfs (file-system
                   (device (uuid "480ebd1b-e5b6-4ff0-a54e-864b15ff9e7d"))
                   (mount-point "/")
                   (type "btrfs")
                   (options "compress=zstd,subvol=System"))))
     (append (list rootfs)

             (list (file-system
                     (inherit rootfs)
                     (mount-point "/var/lib")
                     (check? #f)
                     (options (string-append "compress=zstd,subvol=Data"))))

             %rosenthal-base-file-systems)))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups '("wheel")))
          %base-user-accounts))

  (packages (map normalize-package %rosenthal-base-packages))

  (timezone "Asia/Hong_Kong")

  (services
   (cons* (service cloudflare-tunnel-service-type
                   (cloudflare-tunnel-configuration
                    (token %paradise-cloudflared-token)
                    (http2-origin? #t)))

          (service dbus-root-service-type)

          (service dhcp-client-service-type)

          (service docker-service-type)

          (service elogind-service-type)

          (service (fail2ban-jail-service
                    openssh-service-type
                    (fail2ban-jail-configuration
                     (name "sshd")
                     (enabled? #t)))
                   (openssh-configuration
                    (port-number 54371)
                    (password-authentication? #f)
                    (authorized-keys %paradise-ssh-authorized-keys)))

          (simple-service 'guix-extra-configuration guix-service-type
                          (guix-extension
                           (authorized-keys
                            (list %guix-authorized-key-dorphine))
                           (substitute-urls
                            (list "https://substitute.boiledscript.com"))))

          %rosenthal-base-services)))
