;;; servant.el --- ELPA server written in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (ansi "0.3.0") (commander "0.5.0") (elnode "0.9.9.7.6") (epl "0.2"))

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

;;; Code:

(require 's)
(require 'f)
(require 'ansi)
(require 'dash)
(require 'epl)
(require 'elnode)
(require 'commander)



;;;; Package Index functions

(defun servant-create-index-entry (file-name)
  "Create a package index entry for the package at FILE-NAME.

FILE-NAME can be either an Emacs Lisp file or a tar file with an
Emacs Lisp file or PKG file in it.

Return a package index entry."
  (let ((format (if (string= (f-ext file-name) "tar") 'tar 'single))
        (package (epl-package-from-file file-name)))
    (cons (epl-package-name package)
          (vector (epl-package-version package)
                  (--map (list (epl-requirement-name it)
                               (epl-requirement-version it))
                         (epl-package-requirements package))
                  (epl-package-summary package)
                  format))))

(defun servant-package-file? (file-name)
  "Determine whether FILE-NAME is a package."
  (member (f-ext file-name) '("el" "tar")))

(defun servant-create-index (directory)
  "Generate a package index for DIRECTORY."
  (let* ((package-files (f-files directory #'servant-package-file?))
         (entries (-map 'servant-create-index-entry package-files)))
    (append (list 1) entries)))

(defun servant--create-index-string (directory)
  "Generate a package index for DIRECTORY as string."
  (let ((print-level nil)
        (print-length nil))
    (prin1-to-string (servant-create-index directory))))


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

(defun servant-create-routes (package-directory)
  "Create routes to serve packages from PACKAGE-DIRECTORY."
  (list
   (cons "^.*/archive-contents$" (servant-make-index-handler package-directory))
   (cons "^.*/\\(.*\\)$" (elnode-webserver-handler-maker
                         package-directory
                         '(("application/x-tar" . "tar")
                           ("text/x-emacs-lisp" . "x"))))))

(defun servant-make-elnode-handler (package-directory)
  "Create a handler to serve packages from PACKAGE-DIRECTORY."
  (let ((routes (servant-create-routes package-directory)))
    (lambda (httpcon)
      (elnode-hostpath-dispatcher httpcon routes))))


;;;; Variables

(defvar servant-port 9191
  "Server port.")

(defconst servant-path
  (f-expand "servant")
  "Path to main Servant directory.")

(defconst servant-tmp-path
  (f-expand "tmp" servant-path)
  "Path to tmp directory.")

(defconst servant-packages-path
  (f-expand "packages" servant-path)
  "Path to package directory.")

(defconst servant-index-file
  (f-expand "archive-contents" servant-packages-path)
  "Path to index (archive content) file.")

(defvar servant-pid-file
  (f-expand "servant.pid" servant-tmp-path)
  "Path to server PID file.")

(defconst servant-routes
  (list (cons "^.*//packages/\\(.*\\)$"
              (servant-make-elnode-handler servant-packages-path)))
  "Routes for the built-in local server.")


;;;; Options

(defun servant/pid (pid-file)
  "Set path to PID file."
  (setq servant-pid-file pid-file))

(defun servant/port (port)
  "Set server port."
  (setq servant-port port))

(defun servant/debug ()
  "Enable debug options."
  (setq debug-on-error t)
  (setq debug-on-entry t))


;;;; Commands

(defun servant/help ()
  "Show Servant usage information."
  (commander-print-usage-and-exit))

(defun servant/init ()
  "Initialize the project for Servant."
  (when (f-dir? servant-path)
    (error (ansi-red "Directory `servant` already exists.")))
  (f-mkdir servant-path)
  (f-mkdir servant-tmp-path)
  (f-mkdir servant-packages-path)
  (message "create %s" (ansi-green "servant"))
  (message "create %s" (ansi-green "servant/tmp"))
  (message "create %s" (ansi-green "servant/packages")))

(defun servant/start ()
  "Start server."
  (unless (f-dir? servant-path)
    (error (ansi-red "Servant not initialized, run `servant init`.")))
  (elnode-start (lambda (httpcon)
                  (elnode-hostpath-dispatcher httpcon servant-routes))
                :port servant-port :host "localhost")
  (with-temp-file servant-pid-file
    (insert (format "%s" (emacs-pid))))
  (while t (sit-for 10000)))

(defun servant/stop ()
  "Stop server."
  (elnode-stop servant-port))

(defun servant/index ()
  "Generate index (archive contents) file for all packages."
  (f-write (servant--create-index-string servant-packages-path)
           'utf-8 servant-index-file))


;;;; Commander schema

(commander
 (name "servant")
 (description "Serve ELPA packages")
 (config ".servant")

 (default servant/help)

 (option "-h, --help" "Print usage information" servant/help)
 (option "-p <port>, --port <port>" "Use port (default: 9191)" servant/port)
 (option "-P <file>, --pid <file>" "Name to PID file (default: servant.pid)" servant/pid)
 (option "--debug" "Enable debug information" servant/debug)
 (option "--index" "Index before running command" servant/index)

 (command "init" "Initialize servant" servant/init)
 (command "index" "Build package index" servant/index)
 (command "help" "Print usage information" servant/help)
 (command "start" "Start server" servant/start)
 (command "stop" "Stop server" servant/stop))

(provide 'servant)

;;; servant.el ends here
