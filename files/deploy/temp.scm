;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: CC0-1.0

(use-modules (ice-9 match)
             (gnu machine)
             (gnu machine ssh))

(define %os (load "../../config/temp.scm"))

(map (match-lambda
       ((ip-address system ssh-host-key max-jobs threads-per-job)
        (machine
          (operating-system (%os max-jobs threads-per-job))
          (environment managed-host-environment-type)
          (configuration
           (machine-ssh-configuration
             (host-name ip-address)
             (system system)
             (host-key ssh-host-key)))))
       ((ip-address system ssh-host-key max-jobs threads-per-job bios-boot-disk)
        (machine
          (operating-system (%os max-jobs threads-per-job bios-boot-disk))
          (environment managed-host-environment-type)
          (configuration
           (machine-ssh-configuration
             (host-name ip-address)
             (system system)
             (host-key ssh-host-key))))))
     '(#;("0.0.0.0"
          "x86_64-linux"
          "ssh-ed25519 ..."
          4 2)))
