;; SPDX-FileCopyrightText: 2022, 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-system)
  #:use-module (blobs dorphine)
  #:use-module (testament common)
  #:use-module (testament counter-stop)
  #:use-module (testament kicksecure)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages games)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services security-token)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rosenthal bootloader grub)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages dns)
  #:use-module (rosenthal services bittorrent)
  #:use-module (rosenthal services child-error)
  #:use-module (rosenthal services dns)
  #:use-module (rosenthal services file-systems)
  #:use-module (rosenthal services networking))


;;
;; File-like
;;


(define %config-smartdns
  (let* ((filename "smartdns.conf")
         (conf-dir (file-append dnsmasq-china-list "/share/dnsmasq-china-list/")))
    (mixed-text-file
     filename
     (testament-file-content filename) "\n"
     "conf-file " (file-append conf-dir "accelerated-domains.china.smartdns.conf") "\n"
     "conf-file " (file-append conf-dir "apple.china.smartdns.conf") "\n"
     "conf-file " (file-append conf-dir "bogus-nxdomain.china.smartdns.conf") "\n")))

;; See also: <https://github.com/ValveSoftware/steam-for-linux/issues/2092>
(define %controller-permission-udev-rule
  (udev-rule "60-controller-permission.rules" "\
KERNEL==\"event*\", ATTRS{idVendor}==\"045e\", ATTRS{idProduct}==\"028e\", \
MODE=\"0660\", TAG+=\"uaccess\""))


;;
;; Operating system definition for Dorphine.
;;


(define-public linux-dorphine
  (customize-linux
   #:name "linux-dorphine"
   #:linux linux-xanmod
   #:source linux-xanmod-source
   #:defconfig (testament-file-object "defconfig-zen3-dorphine")
   #:extra-version "dorphine"))

(operating-system
  (kernel linux-dorphine)
  (kernel-arguments
   (cons* "nvidia_drm.modeset=1"
          "zswap.enabled=1"
          "zswap.max_pool_percent=90"
          (string-append "modprobe.blacklist="
                         (string-join
                          (cons* "hid_nintendo"
                                 "nouveau"
                                 "pcspkr"
                                 "snd_pcsp"
                                 (@@ (gnu system) %default-modprobe-blacklist))
                          ","))
          (fold kicksecure-delete
                %kicksecure-kernel-arguments
                '("debugfs" "nosmt"))))

  (bootloader (bootloader-configuration
               (bootloader grub-efi-luks2-removable-bootloader)
               (targets '("/efi"))))

  (keyboard-layout
   (keyboard-layout "us" "dvorak"
                    #:options '("ctrl:nocaps")))

  (initrd
   (lambda (file-systems . rest)
     (apply microcode-initrd
            file-systems
            #:initrd raw-initrd
            #:microcode-packages (list amd-microcode)
            #:helper-packages
            (list btrfs-progs/static
                  fatfsck/static
                  loadkeys-static)
            rest)))

  (initrd-modules
   (cons* "btrfs"
          "xxhash_generic"
          %base-initrd-modules))

  (firmware
   (list amdgpu-firmware
         ibt-hw-firmware
         iwlwifi-firmware))

  (host-name "dorphine")

  (mapped-devices
   (list (mapped-device
	  (source (uuid "9d66aba7-854d-4322-b0ed-e0ab6ad85f00"))
	  (target "Windose")
	  (type luks-device-mapping))))

  (file-systems
   (let ((file-system-base
          (file-system
            (type "btrfs")
            (mount-point "/")
            (device (uuid "86085728-1a31-4c4b-bfd5-ca3bef030136"))
            (create-mount-point? #t)
            (dependencies mapped-devices)))
         (options-for-subvolume
          (cut string-append "compress=zstd,discard=async,subvol=" <>)))
     (append
      (map (match-lambda
             ((subvolume . mount-point)
              (file-system
                (inherit file-system-base)
                (mount-point mount-point)
                (options (options-for-subvolume subvolume))
                (check? (string=? "/" mount-point)))))
           '(("@System/@Guix" . "/")
             ("@Home"         . "/home")
             ("@Swap"         . "/swap")
             ("@Data"         . "/var/lib")))

      ;; Bootloader
      (list (file-system
              (type "vfat")
              (mount-point "/efi")
              (device (uuid "F5BB-DCAF" 'fat))
              (mount? #f)
              (create-mount-point? #t)))

      ;; Devices
      (list (file-system
              (type "btrfs")
              (mount-point "/mnt/Myosotis")
              (device (uuid "537de57b-30b7-4273-868f-83771074b6af"))
              (options "compress=zstd,subvolid=5")
              (mount? #f)
              (create-mount-point? #t))
            (file-system
              (inherit file-system-base)
              (mount-point "/mnt/Mentha")
              (options "compress=zstd,discard=async,subvolid=5")
              (check? #f)
              (create-mount-point? #t)))

      (cons* %debug-file-system
             %testament-base-file-systems))))

  (swap-devices
   (list (swap-space
          (target "/swap/swapfile")
          (dependencies
           (filter (file-system-mount-point-predicate "/swap")
                   file-systems))
          (discard? #t))))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups
            '("audio" "libvirt" "plugdev" "seat" "video" "wheel"))
           (home-directory "/home/hako")
           (shell (file-append bash "/bin/bash")))
          %base-user-accounts))

  (packages
   (cons nss-certs
         (fold delete-package-from-list
               %base-packages
               '(;; From %base-packages-interactive
                 "mg" "nano" "nvi"
                 ;; From %base-packages-networking
                 "isc-dhcp" "iw" "wireless-tools"))))

  (timezone "Asia/Hong_Kong")

  (services
   (cons* (service bluetooth-service-type
                   (bluetooth-configuration
                    (auto-enable? #t)))

          (service btrbk-service-type
                   (btrbk-configuration
                    (config-file
                     (testament-file-object "btrbk-dorphine.conf"))))

          (service clash-service-type
                   (clash-configuration
                    (clash clash-meta-bin)
                    (config (testament-file-object "clash.yaml"))))

          (service greetd-service-type
                   (greetd-configuration
                    (greeter-supplementary-groups
                     '("input" "seat" "video"))
                    (terminals
                     (map (lambda (vtnr)
                            (greetd-terminal-configuration
                             (terminal-vt (number->string vtnr))
                             (terminal-switch (eq? 1 vtnr))
                             (default-session-command
                               (greetd-agreety-session
                                (command (file-append bash "/bin/bash"))))))
                          (iota 6 1)))))

          (service iwd-service-type
                   (iwd-configuration
                    (enable-network-configuration? #t)
                    (address-randomization 'network)))

          (service libvirt-service-type)

          (service nftables-service-type
                   (nftables-configuration
                    (ruleset (testament-file-object "nftables-dorphine.conf"))))

          (service ntp-service-type
                   (ntp-configuration
                    (servers
                     (cons (ntp-server
                            (address "ntp.tuna.tsinghua.edu.cn"))
                           %ntp-servers))))

          (service nvidia-service-type)

          (service pcscd-service-type)

          (service qbittorrent-service-type
                   (qbittorrent-configuration
                    (qbittorrent qbittorrent-enhanced-nox)
                    (webui-port 35951)))

          (service qemu-binfmt-service-type
                   (qemu-binfmt-configuration
                    (platforms (lookup-qemu-platforms "aarch64"))))

          (service seatd-service-type)

          (service screen-locker-service-type
                   (screen-locker-configuration
                    (name "swaylock")
                    (program (plain-file "empty" ""))
                    (using-setuid? #f)))

          (service smartdns-service-type
                   (smartdns-configuration
                    (config-file %config-smartdns)))

          (service tailscale-service-type
                   (tailscale-configuration
                    (iptables iptables-nft)))

          (service tlp-service-type)

          (service udisks-service-type)

          (service virtlog-service-type)

          (service x11-socket-directory-service-type)

          polkit-wheel-service

          (udev-rules-service 'backlight light)
          (udev-rules-service 'controller %controller-permission-udev-rule)
          (udev-rules-service 'steam-devices steam-devices-udev-rules)
          (udev-rules-service 'u2f libfido2 #:groups '("plugdev"))

          (simple-service 'add-extra-hosts hosts-service-type
                          %dorphine-hosts)

          (simple-service 'add-extra-modules kernel-module-loader-service-type
                          '("tcp_bbr"))

          (simple-service 'sysctl-extra-settings sysctl-service-type
                          '(("net.core.default_qdisc" . "fq_pie")
                            ("net.ipv4.tcp_congestion_control" . "bbr")))

          (simple-service 'uaccess-pam-service pam-root-service-type
                          (let ((uaccess-pam-entry
                                 (pam-entry
                                  (control "optional")
                                  (module (file-append
                                           pam-uaccess
                                           "/lib/security/pam_uaccess.so"))
                                  (arguments '("skip_ungrant")))))
                            (list (pam-extension
                                   (transformer
                                    (lambda (pam)
                                      (if (string=? (pam-service-name pam)
                                                    "greetd")
                                          (pam-service
                                           (inherit pam)
                                           (session
                                            (append (pam-service-session pam)
                                                    (list uaccess-pam-entry))))
                                          pam)))))))

          (modify-services %base-services
            (delete login-service-type)
            (delete mingetty-service-type)

            (console-font-service-type
             _ => (map (lambda (vtnr)
                         (let* ((path "/share/consolefonts/ter-132n")
                                (font (file-append font-terminus path))
                                (vtnr (number->string vtnr))
                                (tty (string-append "tty" vtnr)))
                           (cons tty font)))
                       (iota 6 1)))

            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (substitute-urls
                         `("https://mirror.sjtu.edu.cn/guix"
                           ,@%default-substitute-urls
                           "https://substitutes.nonguix.org"
                           "https://substitute.boiledscript.com"))
                        (authorized-keys
                         (cons* %guix-authorized-key-gokuraku
                                %guix-authorized-key-nonguix
                                %default-authorized-guix-keys))
                        (http-proxy "http://127.0.0.1:7890")))

            (sysctl-service-type
             config => (sysctl-configuration
                        (inherit config)
                        (settings
                         (fold kicksecure-delete
                               %kicksecure-sysctl-rules
                               '("kernel.sysrq"
                                 "net.ipv4.icmp_echo_ignore_all"
                                 "vm.swappiness")))))))))
