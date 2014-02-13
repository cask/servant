;;; servant.el --- ELPA server written in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <lunaryorn@gmail.com>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Version: 0.0.2
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (ansi "0.3.0") (commander "0.5.0") (elnode "0.9.9.7.6") (epl "0.2") (shut-up "0.2.1"))

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

;; An ELPA server for Elnode.

;; Serves ELPA packages from ELPA. Builds the package index on the fly, if no
;; index file is present.

;; `servant-make-elnode-handler' creates an Elnode handler to serve packages
;; from a directory.  `servant-create-index' creates the index.

;;; Code:

(require 's)
(require 'f)
(require 'dash)
(require 'epl)
(require 'elnode)


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
    (concat "\n" (prin1-to-string (servant-create-index directory)))))


;;;; Generic elnode handlers
(defun servant-make-index-handler (package-directory)
  "Create a handler to serve the index for PACKAGE-DIRECTORY.

If PACKAGE-DIRECTORY has no index file, return an in-memory
index, which is auto-generated on the fly."
  (let ((index-file (f-join package-directory "archive-contents")))
    (lambda (httpcon)
      (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
      (if (f-exists? index-file)
          (elnode-send-file httpcon index-file)
        (elnode-http-return
         httpcon (servant--create-index-string package-directory))))))

(defun servant-make-package-handler (package-directory)
  "Create a handler to serve packages from PACKAGE-DIRECTORY.

This handler sends proper HTTP responses for package files in
PACKAGE-DIRECTORY."
  (lambda (httpcon)
    (elnode-docroot-for package-directory
      with target
      on httpcon
      do
      (if (f-directory? target)
          ;; Generate the index
          (let* ((pathinfo (elnode-http-pathinfo httpcon))
                 (index (elnode--webserver-index package-directory target
                                                 pathinfo)))
            (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
            (elnode-http-return httpcon index))
        (let ((mimetype (cl-case (servant-package-type target)
                          (tar "application/x-tar")
                          (single "text/x-emacs-lisp")
                          (otherwise "application/octet-stream")))
              (content (string-to-multibyte (f-read-bytes target))))
          (elnode-http-start httpcon 200
                             (cons "Content-Type" mimetype)
                             (cons "Content-Length" (length content)))
          (elnode-http-return httpcon content))))))

(defun servant-create-routes (package-directory)
  "Create routes to serve packages from PACKAGE-DIRECTORY."
  (list
   (cons "^.*/archive-contents$" (servant-make-index-handler package-directory))
   ;; Oh, geez.  We used to use `elnode-webserver-handler-maker' here, because
   ;; elnode conveniently handles all the nasty details of serving files from a
   ;; directory, but stupid silly package.el can't handle chunked transfer
   ;; encoding.  It simply reads to the end of the connection, so the final EOF
   ;; of chunked transfers makes it into the package file, breaking extraction
   ;; of tars and loading of singles.  So for the sake of playing nice with
   ;; package.el, we use our own handler, that sends standard HTTP replies.
   (cons "^.*/\\([^/]*\\)$" (servant-make-package-handler package-directory))
   ))

(defun servant-make-elnode-handler (package-directory)
  "Create a handler to serve packages from PACKAGE-DIRECTORY."
  (let ((routes (servant-create-routes package-directory)))
    (lambda (httpcon)
      (elnode-hostpath-dispatcher httpcon routes))))

(provide 'servant)

;;; servant.el ends here
