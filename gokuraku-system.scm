(define-module (gokuraku-system)
  #:use-module (blobs gokuraku)
  #:use-module (testament common)
  #:use-module (testament counter-stop)
  #:use-module (testament kicksecure)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
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
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system shadow)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rosenthal services child-error)
  #:use-module (rosenthal services file-systems)
  #:use-module (rosenthal services networking))

(operating-system
  (kernel linux-xanmod)

  (kernel-arguments
   (cons* "gather_data_sampling=force"
          "zswap.enabled=1"
          "zswap.max_pool_percent=90"
          (append %default-kernel-arguments
                  (fold kicksecure-delete
                        %kicksecure-kernel-arguments
                        '("l1tf"
                          "mds"
                          "mitigations"
                          "mmio_stale_data"
                          "nosmt")))))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/nvme1n1"))))

  (initrd
   (lambda (file-systems . rest)
     (apply microcode-initrd
            file-systems
            #:microcode-packages (list intel-microcode)
            rest)))

  (firmware '())

  (host-name "gokuraku")

  (file-systems
   (let ((file-system-base
          (file-system
            (type "btrfs")
            (mount-point "/")
            (device (uuid "0c7041a2-416f-419f-9b1a-cceae3330aae"))
            (create-mount-point? #t)))
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
           '(("@System" . "/")
             ("@Data"   . "/var/lib")))

      (list (file-system
              (inherit file-system-base)
              (mount-point "/mnt/Lycoris")
              (options "compress=zstd,discard=async,subvolid=5")
              (check? #f)
              (create-mount-point? #t)))

      %testament-base-file-systems)))

  (swap-devices
   (list (swap-space
          (target (uuid "07f2b4fb-3d4e-4a0a-9953-c4200ebdf245"))
          (discard? #t))))

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
          (fold delete-package-from-list
                %base-packages
                '(;; From %base-packages-interactive
                  "mg" "nano" "nvi"
                  ;; From %base-packages-networking
                  "isc-dhcp" "iw" "wireless-tools"))))

  (timezone "Asia/Hong_Kong")

  (services
   (cons* (service btrbk-service-type
                   (btrbk-configuration
                    (config-file
                     (testament-file-object "btrbk-gokuraku.conf"))))

          (service cloudflare-tunnel-service-type
                   (cloudflare-tunnel-configuration
                    (token %gokuraku-cloudflared-token)))

          (service dbus-root-service-type)

          (service elogind-service-type)

          (service guix-publish-service-type
                   (guix-publish-configuration
                    (port 27254)))

          (service openssh-service-type
                   %gokuraku-ssh-configuration)

          (service ntp-service-type)

          (service static-networking-service-type
                   %gokuraku-static-networks)

          (service tailscale-service-type
                   (tailscale-configuration
                    (iptables iptables-nft)))

          (simple-service 'add-extra-modules kernel-module-loader-service-type
                          '("tcp_bbr"))

          (simple-service 'sysctl-extra-settings sysctl-service-type
                          '(("net.core.default_qdisc" . "fq_pie")
                            ("net.ipv4.tcp_congestion_control" . "bbr")))

          (modify-services %base-services
            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (authorized-keys
                         (cons* %guix-authorized-key-dorphine
                                %default-authorized-guix-keys))))

            (sysctl-service-type
             config => (sysctl-configuration
                        (inherit config)
                        (settings
                         (fold kicksecure-delete
                               %kicksecure-sysctl-rules
                               '("net.core.bpf_jit_harden"
                                 "net.ipv4.icmp_echo_ignore_all"
                                 "vm.swappiness"))))))))

  (sudoers-file %gokuraku-sudoers-file))
