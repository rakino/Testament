;; SPDX-FileCopyrightText: 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (testament common)
  #:use-module ((guix diagnostics) #:select (leave))
  #:use-module ((guix gexp) #:select (local-file))
  #:use-module ((guix i18n) #:select (G_))
  #:use-module ((guix packages) #:select (package-name))
  #:use-module ((rnrs io ports) #:select (get-string-all))
  #:export (testament-find-file
            testament-file-content
            testament-file-object

            delete-package-from-list))

(define (testament-path)
  "Get the path to Testament repository, fallback to \"~/Workspace/Testament\"
in REPL."
  (dirname
   (dirname
    (or (current-filename)
        (string-append (getenv "HOME")
                       "/Workspace/Testament/testament/common.scm")))))

(define (testament-find-file name)
  "Find a file in the \"files\" directory (fallback to \"blobs\") of Testament
repository.  Return a string of path to the file, or #f if file not found."
  (let ((file-name (string-append (testament-path) "/files/" name))
        (blob-name (string-append (testament-path) "/blobs/" name)))
    (cond ((file-exists? file-name) file-name)
          ((file-exists? blob-name) blob-name)
          (else (leave (G_ "file '~a' not found.~%") name)))))

(define (testament-file-content name)
  "Return a string, the content of file NAME located in the \"files\" or
\"blobs\" directory of Testament repository."
  (call-with-input-file (testament-find-file name)
    (lambda (port)
      (get-string-all port))))

(define (testament-file-object name)
  "Similar to 'testament-file-content' but return a file-like object."
  (local-file (testament-find-file name)))

(define (delete-package-from-list name lst)
  "Return a copy of package list LST, removing packages named NAME."
  (filter (lambda (pkg)
            (not (string=? name (package-name pkg))))
          lst))
