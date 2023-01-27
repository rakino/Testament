;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (common)
  #:use-module ((ice-9 rdelim) #:select (read-delimited))
  #:use-module ((guix gexp) #:select (local-file))
  #:export (agathion
            nohitaga
            summon))

(define (testament-path)
  "Get the path to Testament repository, fallback to \"~/Workspace/Testament\"
in REPL."
  (dirname (or (current-filename)
               (string-append (getenv "HOME")
                              "/Workspace/Testament/common.scm"))))

(define (summon name)
  "Find a file in the \"files\" directory (fallback to \"blobs\") of Testament
repository.  Return a string of path to the file, or #f if file not found."
  (let ((file-name (string-append (testament-path) "/files/" name))
        (blob-name (string-append (testament-path) "/blobs/" name)))
    (cond ((file-exists? file-name) file-name)
          ((file-exists? blob-name) blob-name)
          (else (and (format (current-error-port)
                             "File `~a' not found.~%"
                             name)
                     #f)))))

(define (agathion name)
  "Return a string, the content of file NAME located in the \"files\" or
\"blobs\" directory of Testament repository."
  (call-with-input-file (summon name)
    (lambda (port)
      (read-delimited "" port))))

(define (nohitaga name)
  "Similar to 'agathion' but return a file-like object."
  (local-file (summon name)))
