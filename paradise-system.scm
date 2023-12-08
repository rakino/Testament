;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (paradise-system)
  #:use-module (blobs paradise)
  #:use-module (testament counter-stop)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal services child-error))


;;
;; Operating system definition for Paradise.
;;


(define %paradise-initrd-modules
  (append '("virtio_scsi")
          %testament-base-initrd-modules
          %base-initrd-modules))

(define %paradise-packages
  (append (list git mosh rsync zstd)
          %testament-base-packages))

(operating-system
  (kernel-arguments %testament-default-kernel-arguments)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (initrd (lambda (file-systems . rest)
            (raw-initrd file-systems
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
                     (options "compress=zstd,subvol=Data")))

             %testament-base-file-systems)))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups '("wheel")))
          %base-user-accounts))

  (packages (map normalize-package %paradise-packages))

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
                    (openssh openssh-sans-x)
                    (port-number 54371)
                    (password-authentication? #f)
                    (authorized-keys %paradise-ssh-authorized-keys)))

          (service zram-device-service-type
                   (zram-device-configuration
                    (size "2G")))

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

  (sudoers-file %paradise-sudoers-file))
