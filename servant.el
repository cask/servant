;;; servant.el --- ELPA server written in Emacs Lisp

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (ansi "0.3.0") (commander "0.5.0") (elnode "0.9.9.7.6") (epl "0.1"))

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
  (f-expand "index" servant-path)
  "Path to index (archive context) file.")

(defconst servant-package-re
  "\/\\([^/]+\\)-\\([^/]+\\)\.\\(tar\\|el\\)$"
  "Regular expression matching a package file name.")

(defvar servant-pid-file
  (f-expand "servant.pid" servant-tmp-path)
  "Path to server PID file.")

(defconst servant-routes
  '(("\/packages\/archive-contents$" . servant--archive-handler)
    ("\/packages\/\\(.+\\)-\\(.+\\)\.\\(tar\\|el\\)$" . servant--package-handler)
    ("\/.*" . servant--default-handler))
  "Server routes.")


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
  (unless (f-file? servant-index-file)
    (error (ansi-red "No index, run `servant index` to create")))
  (elnode-start 'servant--root-handler :port servant-port :host "localhost")
  (with-temp-file servant-pid-file
    (insert (format "%s" (emacs-pid))))
  (while t (sit-for 10000)))

(defun servant/stop ()
  "Stop server."
  (elnode-stop servant-port))

(defun servant/index ()
  "Generate index (archive contents) file for all packages."
  (let* ((package-files (f--files servant-packages-path (s-matches? servant-package-re it)))
         (packages (-map 'servant--package-info package-files)))
    (f-write
     (format
      "(1\n %s)"
      (s-join
       "\n "
       (--map (apply 'format "(%s . [%s %s \"%s\" %s])" it) packages)))
     'utf-8 servant-index-file)))


;;;; Helper functions

(defun servant--package-info (filename)
  "Get package info from FILENAME.

FILENAME can be either an Emacs Lisp file or a tar file with an
Emacs Lisp file or PKG file in it.

Result is a list of the form: (name version requires description format)"
  (let ((ext (f-ext filename)))
    (cond ((equal ext "el")
           (servant--el-package-info filename))
          ((equal ext "tar")
           (servant--tar-package-info filename)))))

(defun servant--el-package-info (filename)
  "Get package information from main Emacs Lisp file."
  (let ((format (intern (f-ext filename)))
        (package (epl-package-from-file filename)))
    (list
     (epl-package-name package)
     (epl-package-version package)
     (--map (list (epl-requirement-name it)
                  (epl-requirement-version-string it))
            (epl-package-requirements package))
     (epl-package-summary package)
     format)))

(defun servant--pkg-package-info (filename)
  "Get package information from PKG file."
  (let ((info (cdr (read (f-read filename)))))
    (list
     (intern (nth 0 info))
     (version-to-list (nth 1 info))
     (mapcar
      (lambda (elt)
        (list (car elt) (version-to-list (cadr elt))))
      (eval (nth 3 info)))
     (nth 2 info)
     (f-ext filename))))

(defun servant--tar-package-info (filename)
  "Get package information from Tar file."
  (let* ((matches (s-match servant-package-re filename))
         (name (nth 1 matches))
         (version (nth 2 matches))
         (extract-to (f-expand (concat name "-" version) servant-tmp-path)))
    (when (f-dir? extract-to)
      (f-delete extract-to :force))
    (f-mkdir extract-to)
    (with-temp-buffer
      (when (= (call-process (executable-find "tar") nil (current-buffer) nil "-xf" filename "-C" extract-to) 1)
        (error (buffer-string))))
    (let ((pkg-file (f-expand (concat name "-pkg.el") extract-to)))
      (if (f-file? pkg-file)
          (servant--pkg-package-info pkg-file)
        (let ((main-file (f-expand (concat name ".el") extract-to)))
          (when (f-file? main-file)
            (servant--el-package-info main-file)))))))


;;;; ELnode handlers

(defun servant--root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon servant-routes))

(defun servant--archive-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon (f-read servant-index-file)))

(defun servant--package-handler (httpcon)
  (let* ((name (elnode-http-mapping httpcon 1))
         (version (elnode-http-mapping httpcon 2))
         (format (elnode-http-mapping httpcon 3))
         (package-file
          (f-expand (concat name "-" version "." format) servant-packages-path)))
    (cond ((f-file? package-file)
           (let* ((content (f-read package-file))
                  (content-type
                   (if (equal format "el")
                       "application/octet-stream"
                     "application/x-tar"))
                  (content-length (length content)))
             (elnode-http-start httpcon 404
                                `("Content-type" . ,content-type)
                                `("Content-length" . ,content-length))
             (elnode-http-return httpcon content)))
          (t
           (elnode-http-start httpcon 404 '("Content-type" . "text/plain"))
           (elnode-http-return httpcon (format "Package `%s` not found" name))))))

(defun servant--default-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon ""))


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
