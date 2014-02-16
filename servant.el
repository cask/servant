;;; servant.el --- ELPA server written in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <lunaryorn@gmail.com>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Version: 0.3.0
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (ansi "0.3.0") (commander "0.5.0") (epl "0.2") (shut-up "0.2.1") (web-server "0.0.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; An ELPA server.

;; Serves ELPA packages from ELPA. Builds the package index on the fly, if no
;; index file is present.

;;; Code:

(require 's)
(require 'f)
(require 'dash)
(require 'epl)


;;;; Package Index functions

(defun servant-create-index-entry (file-name)
  "Create a package index entry for the package at FILE-NAME.

FILE-NAME can be either an Emacs Lisp file or a tar file with an
Emacs Lisp file or PKG file in it.

Return a package index entry."
  (-when-let* ((format (servant-package-type file-name))
               (package (epl-package-from-file file-name)))
    (cons (epl-package-name package)
          (vector (epl-package-version package)
                  (--map (list (epl-requirement-name it)
                               (epl-requirement-version it))
                         (epl-package-requirements package))
                  (epl-package-summary package)
                  format))))

(defun servant-package-type (file-name)
  "Determine the package type of FILE-NAME.

Return `tar' for tarball packages, `single' for single file
packages, or nil, if FILE-NAME is not a package."
  (let ((ext (f-ext file-name)))
    (cond
     ((string= ext "tar") 'tar)
     ((string= ext "el") 'single)
     (:else nil))))

(defun servant-create-index (directory)
  "Generate a package index for DIRECTORY."
  (let* ((package-files (f-files directory #'servant-package-type))
         (entries (-map 'servant-create-index-entry package-files)))
    (append (list 1) entries)))

(defun servant--create-index-string (directory)
  "Generate a package index for DIRECTORY as string."
  (let ((print-level nil)
        (print-length nil))
    (concat "\n" (prin1-to-string (servant-create-index directory)) "\n")))

(defun servant-start ()
  "Start server."
  (unless (f-dir? (servant-path))
    (error (ansi-red "Servant not initialized, run `servant init`.")))
  (let ((docroot (servant-path)))
    (ws-start
     (lambda (request)
       (with-slots (process headers) request
         (let ((path (substring (cdr (assoc :GET headers)) 1)))
           (if (ws-in-directory-p docroot path)
               (if (f-dir? path)
                   (ws-send-directory-list process (f-expand path docroot) "^[^\.]")
                 (ws-send-file process (f-expand path docroot)))
             (ws-send-404 process)))))
     servant-port))
  (with-temp-file (servant-pid-file)
    (insert (format "%s" (emacs-pid)))))

(defun servant-stop ()
  "Stop server."
  (when (f-file? (servant-pid-file))
    (let ((pid (f-read-text (servant-pid-file))))
      (with-temp-buffer
        (let ((exit-code (call-process "kill" nil t nil pid)))
          (unless (zerop exit-code)
            (error (buffer-string))))))))

(provide 'servant)

;;; servant.el ends here
