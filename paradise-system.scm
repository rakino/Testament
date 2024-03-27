;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (paradise-system)
  #:use-module (blobs paradise)
  #:use-module (testament counter-stop)
  #:use-module (testament kicksecure)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
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


(operating-system
  (kernel-arguments %kicksecure-kernel-arguments)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))))

  (initrd
   (lambda (file-systems . rest)
     (apply raw-initrd
            file-systems
            #:helper-packages (list btrfs-progs/static)
            rest)))

  (initrd-modules
   (cons* "btrfs"
          "virtio_scsi"
          "xxhash_generic"
          %base-initrd-modules))

  (firmware '())

  (host-name "paradise")

  (file-systems
   (let ((file-system-base
          (file-system
            (type "btrfs")
            (mount-point "/")
            (device (uuid "480ebd1b-e5b6-4ff0-a54e-864b15ff9e7d"))
            (create-mount-point? #t)))
         (options-for-subvolume
          (cut string-append "compress=zstd,subvol=" <>)))
     (append
      (map (match-lambda
             ((subvolume . mount-point)
              (file-system
                (inherit file-system-base)
                (mount-point mount-point)
                (options (options-for-subvolume subvolume))
                (check? (string=? "/" mount-point)))))
           '(("System" . "/")
             ("Data"   . "/var/lib")))

      %testament-base-file-systems)))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups '("wheel")))
          %base-user-accounts))

  (packages
   (cons* mosh
          nss-certs
          rsync
          zstd
          %base-packages))

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
                   %paradise-ssh-configuration)

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

          (modify-services %base-services
            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (substitute-urls
                         `(,@%default-substitute-urls
                           "https://substitute.boiledscript.com"))
                        (authorized-keys
                         (cons* %guix-authorized-key-dorphine
                                %guix-authorized-key-gokuraku
                                %default-authorized-guix-keys))))

            (sysctl-service-type
             config => (sysctl-configuration
                        (inherit config)
                        (settings %kicksecure-sysctl-rules))))))

  (sudoers-file %paradise-sudoers-file))
