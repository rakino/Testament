;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (dorphine-system)
  #:use-module (blobs dorphine)
  #:use-module ((testament common)
                #:select (testament-find-file
                          (testament-file-content . agathion)
                          (testament-file-object . nohitaga)))
  #:use-module (testament counter-stop)
  #:use-module (ice-9 match)
  #:use-module (gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages wm)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services security-token)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages dns)
  #:use-module (rosenthal services bittorrent)
  #:use-module (rosenthal services child-error)
  #:use-module (rosenthal services dns)
  #:use-module (rosenthal services networking))


;;
;; File-like
;;


(define %config-smartdns
  (let* ((filename "smartdns.conf")
         (conf-dir (file-append dnsmasq-china-list "/share/dnsmasq-china-list/")))
    (mixed-text-file
     filename
     (agathion filename) "\n"
     "conf-file " (file-append conf-dir "accelerated-domains.china.smartdns.conf") "\n"
     "conf-file " (file-append conf-dir "apple.china.smartdns.conf") "\n"
     "conf-file " (file-append conf-dir "bogus-nxdomain.china.smartdns.conf") "\n")))


;;
;; Mcron jobs
;;


(define %mcron-job-defrag-guix-db
  #~(job next-day-from
         #$(file-append btrfs-progs "/bin/btrfs fi defrag -r /var/guix/db/")))


;;
;; Operating system definition for Dorphine.
;;


(define-public linux-dorphine
  (customize-linux
   #:name "linux-dorphine"
   #:linux linux-xanmod
   #:source linux-xanmod-source
   #:defconfig (nohitaga "defconfig-zen3-dorphine")
   #:extra-version "dorphine"))

(define %dorphine-initrd-modules
  (append %testament-base-initrd-modules
          %base-initrd-modules))

(operating-system
  (kernel linux-dorphine)
  (kernel-arguments `(,@%testament-default-kernel-arguments
                      "modprobe.blacklist=hid_nintendo,nouveau,pcspkr,snd_pcsp"))

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets '("/efi"))
               (menu-entries
                (list (menu-entry
                       (label "Microsoft Windows 11")
                       (device "/dev/nvme0n1p1")
                       (chain-loader "/EFI/Microsoft/Boot/bootmgfw.efi"))))
               (theme
                (grub-theme
                 (image (nohitaga "grub.png"))
                 (resolution '(2560 . 1600))))))

  (keyboard-layout %testament-default-keyboard-layout)

  (initrd (lambda (file-systems . rest)
            (microcode-initrd file-systems
                              #:initrd raw-initrd
                              #:microcode-packages (list amd-microcode)
                              #:keyboard-layout keyboard-layout
                              #:linux kernel
                              #:linux-modules %dorphine-initrd-modules
                              #:helper-packages
                              (list btrfs-progs/static
                                    fatfsck/static
                                    loadkeys-static))))

  (initrd-modules %dorphine-initrd-modules)

  (firmware
   (list amdgpu-firmware
         ibt-hw-firmware
         iwlwifi-firmware))

  (host-name "dorphine")

  (file-systems
   (let ((rootfs (file-system
                   (device (uuid "d6a4de85-7276-4573-aa94-e8d3927585ae"))
                   (mount-point "/")
                   (type "btrfs")
                   (options "compress=zstd,discard=async,subvol=Skeleton/Guix"))))
     (append (list rootfs)

             ;; Bootloader
             (list (file-system
                     (device (uuid "6985-D4C6" 'fat))
                     (mount-point "/efi")
                     (type "vfat")
                     (mount? #f))
                   (file-system
                     (inherit rootfs)
                     (mount-point "/boot")
                     (options "compress=zstd,discard=async,subvol=Boot")))

             (map (match-lambda
                    ((mount-point subvolume)
                     (file-system
                       (inherit rootfs)
                       (mount-point mount-point)
                       (check? #f)
                       (options (string-append "compress=zstd,discard=async,subvol=" subvolume)))))
                  '(("/home"    "Home")
                    ("/var/lib" "Data")))

             ;; Devices
             (list (file-system
                     (inherit rootfs)
                     (mount-point "/mnt/Phinix")
                     (check? #f)
                     (options "compress=zstd,discard=async,subvolid=5")
                     (mount? #f))
                   (file-system
                     (device (uuid "4E21-0000" 'fat))
                     (mount-point "/mnt/Symphytum")
                     (type "exfat")
                     (mount? #f)))

             %testament-base-file-systems)))

  (users
   (cons* (user-account
           (name "hako")
           (group "users")
           (supplementary-groups
            '("plugdev" "seat" "audio" "video" "wheel"))
           (home-directory "/home/hako")
           (shell (file-append bash "/bin/bash")))
          %base-user-accounts))

  (packages (map normalize-package %testament-base-packages))

  (timezone "Asia/Hong_Kong")

  (services
   (cons* (service bluetooth-service-type)

          (service clash-service-type
                   (clash-configuration
                    (clash clash-meta-bin)
                    (log-file "/var/log/clash-tor.log")
                    (data-directory "/var/lib/clash-tor")
                    (config (nohitaga "clash-tor.yaml"))
                    (shepherd-provision '(clash-tor))))

          (service clash-service-type
                   (clash-configuration
                    (clash clash-meta-bin)
                    (config (nohitaga "clash.yaml"))))

          (service cloudflare-tunnel-service-type
                   (cloudflare-tunnel-configuration
                    (token %dorphine-cloudflared-token)
                    (http2-origin? #t)))

          (service greetd-service-type
                   (greetd-configuration
                    (greeter-supplementary-groups
                     '("input" "video" "seat"))
                    (terminals
                     (map (lambda (vtnr)
                            (greetd-terminal-configuration
                             (terminal-vt (number->string vtnr))
                             (terminal-switch (eq? 1 vtnr))
                             (default-session-command
                               (greetd-agreety-session
                                (command (file-append bash "/bin/bash"))))))
                          (iota 6 1)))))

          (service guix-publish-service-type
                   (guix-publish-configuration
                    (port 27254)))

          (service iwd-service-type
                   (iwd-configuration
                    (enable-network-configuration? #t)
                    (address-randomization 'once)))

          (service mcron-service-type
                   (mcron-configuration
                    (jobs (list %mcron-job-defrag-guix-db))))

          (service nftables-service-type
                   (nftables-configuration
                    (ruleset (nohitaga "nftables-dorphine.conf"))))

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
                    (program (file-append swaylock-effects "/bin/swaylock"))
                    (allow-empty-password? #f)
                    (using-pam? #t)
                    (using-setuid? #f)))

          (service smartdns-service-type
                   (smartdns-configuration
                    (config-file %config-smartdns)))

          (service tailscale-service-type)

          (service tor-service-type
                   (tor-configuration
                    (config-file (nohitaga "tor.conf"))))

          (service zram-device-service-type
                   (zram-device-configuration
                    (size "6G")))

          (udev-rules-service 'backlight light)
          (udev-rules-service 'u2f libfido2 #:groups '("plugdev"))

          (simple-service 'add-extra-hosts hosts-service-type
                          %dorphine-hosts)

          (simple-service 'add-extra-modules kernel-module-loader-service-type
                          '("tcp_bbr"))

          (simple-service 'setup-etc-dir etc-service-type
                          `(("btrbk/btrbk.conf" ,(nohitaga "btrbk-dorphine.conf"))))

          (simple-service 'sysctl-extra-settings sysctl-service-type
                          '(("net.core.default_qdisc" . "fq_pie")
                            ("net.ipv4.tcp_congestion_control" . "bbr")
                            ;; Pop!_OS settings for zram
                            ("vm.page-cluster" . "0")
                            ("vm.swappiness" . "180")
                            ("vm.watermark_boost_factor" . "0")
                            ("vm.watermark_scale_factor" . "125")))

          (modify-services %testament-base-services
            (delete login-service-type)
            (delete mingetty-service-type)

            (console-font-service-type
             config => (map (lambda (vtnr)
                              (let* ((path "/share/consolefonts/ter-132n")
                                     (font (file-append font-terminus path))
                                     (vtnr (number->string vtnr))
                                     (tty (string-append "tty" vtnr)))
                                `(,tty . ,font)))
                            (iota 6 1)))

            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (substitute-urls
                         `("https://mirror.sjtu.edu.cn/guix"
                           ,@(guix-configuration-substitute-urls config)
                           "https://substitutes.nonguix.org"))
                        (authorized-keys
                         (cons* %guix-authorized-key-nonguix
                                (guix-configuration-authorized-keys config)))))))))
