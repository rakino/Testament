;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (kroan-system)
  #:use-module (testament common)
  #:use-module (testament counter-stop)
  #:use-module (gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (rosenthal packages linux)
  #:use-module (rosenthal services child-error))

(load (testament-find-file "blob-kroan"))


;;
;; Operating system definition for Kroan.
;;


(define %kroan-initrd-modules
  (append '("virtio_scsi")
          %testament-base-initrd-modules
          %base-initrd-modules))

(define %kroan-packages
  (append (list git mosh rsync zstd)
          %testament-base-packages))

(operating-system
  (kernel-arguments %testament-default-kernel-arguments)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (initrd (lambda (file-systems . rest)
            (raw-initrd file-systems
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

             %testament-base-file-systems)))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups '("wheel")))
          %base-user-accounts))

  (packages (map normalize-package %kroan-packages))

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
                    (openssh openssh-sans-x)
                    (port-number 51048)
                    (password-authentication? #f)
                    (authorized-keys %kroan-ssh-authorized-keys)))

          (service static-networking-service-type
                   %kroan-static-networking-configuration)

          (service zram-device-service-type
                   (zram-device-configuration
                    (size "6G")))

          (simple-service 'add-extra-modules kernel-module-loader-service-type
                          '("tcp_bbr"))

          (simple-service 'sysctl-extra-settings sysctl-service-type
                          '(("net.core.default_qdisc" . "fq_codel")
                            ("net.ipv4.tcp_congestion_control" . "bbr")
                            ("vm.overcommit_memory" . "1")
                            ;; Pop!_OS settings for zram
                            ("vm.page-cluster" . "0")
                            ("vm.swappiness" . "180")
                            ("vm.watermark_boost_factor" . "0")
                            ("vm.watermark_scale_factor" . "125")))

          (modify-services %testament-base-services
            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (substitute-urls
                         (append (guix-configuration-substitute-urls config)
                                 '("https://substitute.boiledscript.com")))
                        (authorized-keys
                         (cons* %guix-authorized-key-dorphine
                                (guix-configuration-authorized-keys config))))))))

  (sudoers-file %kroan-sudoers-file))
