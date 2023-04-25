;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (kroan-system)
  #:use-module (common)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rosenthal packages linux)
  #:use-module (rosenthal services child-error)
  #:use-module (rosenthal utils counter-stop))

(load (summon "blob-kroan-system.scm"))


;;
;; Operating system definition for Kroan.
;;


(define %kroan-initrd-modules
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
            (microcode-initrd file-systems
                              #:initrd raw-initrd
                              #:microcode-packages (list intel-microcode)
                              #:linux kernel
                              #:linux-modules %kroan-initrd-modules
                              #:helper-packages (list btrfs-progs/static))))

  (initrd-modules %kroan-initrd-modules)

  (firmware '())

  (host-name "kroan")

  (file-systems
   (let ((rootfs (file-system
                   (device (uuid "78af006d-0b5f-4b48-8cf3-e5e6d11c0515"))
                   (mount-point "/")
                   (type "btrfs")
                   (options "compress=zstd,discard=async,subvol=System"))))
     (append (list rootfs)

             (list (file-system
                     (inherit rootfs)
                     (mount-point "/var/lib")
                     (check? #f)
                     (options (string-append "compress=zstd,discard=async,subvol=Data"))))

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
                    (token %kroan-cloudflared-token)
                    (http2-origin? #t)))

          (service dbus-root-service-type)

          (service docker-service-type)

          (service elogind-service-type)

          (service (fail2ban-jail-service
                    openssh-service-type
                    (fail2ban-jail-configuration
                     (name "sshd")
                     (enabled? #t)))
                   (openssh-configuration
                    (port-number 51048)
                    (password-authentication? #f)
                    (authorized-keys %kroan-ssh-authorized-keys)))

          (service static-networking-service-type
                   %kroan-static-networking-configuration)

          (service zram-device-service-type)

          (simple-service 'guix-extra-configuration guix-service-type
                          (guix-extension
                           (authorized-keys
                            (list %guix-authorized-key-dorphine))
                           (substitute-urls
                            (list "https://substitute.boiledscript.com"))))

          (simple-service 'sysctl-extra-settings sysctl-service-type
                          '(("vm.overcommit_memory" . "1")))

          %rosenthal-base-services))

  (sudoers-file %kroan-sudoers-file))
