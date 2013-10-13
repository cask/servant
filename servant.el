;;; servant.el --- Serve ELPA packages

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (ansi "0.3.0") (commander "0.5.0") (elnode "0.9.9.7.6"))

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
(require 'elnode)
(require 'package)
(require 'commander)

(defvar servant-port 9191)

(defconst servant-path
  (f-expand "servant"))

(defconst servant-tmp-path
  (f-expand "tmp" servant-path))

(defconst servant-packages-path
  (f-expand "packages" servant-path))

(defconst servant-index-file
  (f-expand "index" servant-path))

(defconst servant-package-re
  "\/\\([^/]+\\)-\\(.+\\)\.\\(tar\\|el\\)$")

(defvar servant-pid-file
  (f-expand "servant.pid" servant-tmp-path))

(defconst servant-routes
  '(("\/packages\/archive-contents$" . servant--archive-handler)
    ("\/packages\/\\(.+\\)-\\(.+\\)\.\\(tar\\|el\\)$" . servant--package-handler)
    ("\/.*" . servant--default-handler)))

(defun servant/pid (pid)
  (setq servant-pid-file pid))

(defun servant/port (port)
  (setq servant-port port))

(defun servant/help ()
  (commander-print-usage-and-exit))

(defun servant/debug ()
  (setq debug-on-error t)
  (setq debug-on-entry t))

(defun servant/init ()
  (when (f-dir? servant-path)
    (error (ansi-red "Directory `servant` already exists.")))
  (f-mkdir servant-path)
  (f-mkdir servant-tmp-path)
  (f-mkdir servant-packages-path)
  (message "create %s" (ansi-green "servant"))
  (message "create %s" (ansi-green "servant/tmp"))
  (message "create %s" (ansi-green "servant/packages")))

(defun servant/start ()
  (unless (f-dir? servant-path)
    (error (ansi-red "Servant not initialized, run `servant init`.")))
  (unless (f-file? servant-index-file)
    (error (ansi-red "No index, run `servant index` to create")))
  (elnode-start 'servant--root-handler :port servant-port :host "localhost")
  (with-temp-file servant-pid-file
    (insert (format "%s" (emacs-pid))))
  (while t (sit-for 10000)))

(defun servant-stop ()
  (elnode-stop servant-port))

(defun servant/index ()
  (let* ((package-files (f--files servant-packages-path (s-matches? servant-package-re it)))
         (packages
          (-map
           (lambda (package-file)
             (with-temp-buffer
               (insert (f-read package-file))
               (let* ((matches (s-match servant-package-re package-file))
                      (info (package-buffer-info))
                      (name (intern (nth 1 matches)))
                      (version (version-to-list (nth 2 matches)))
                      (requires (aref info 1))
                      (description (aref info 2))
                      (format (intern (nth 3 matches))))
                 (list name version requires description format))))
           package-files)))
    (f-write
     (format
      "(1\n %s)"
      (s-join
       "\n "
       (--map (apply 'format "(%s . [%s %s \"%s\" %s])" it) packages)))
     'utf-8 servant-index-file)))

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

(commander
 (name "servant")
 (description "Serve ELPA packages")

 (default servant/help)

 (option "-h, --help" "Print usage information" servant/help)
 (option "-p <port>, --port <port>" "Use port (default: 9191)" servant/port)
 (option "-P <file>, --pid <file>" "File to store PID (default: servant.pid)" servant/pid)
 (option "--debug" "Enable debug information" servant/debug)
 (option "--index" "Index before running command" servant/index)

 (command "init" "Initialize servant" servant/init)
 (command "index" "Build package index" servant/index)
 (command "help" "Print usage information" servant/help)
 (command "start" "Start server" servant/start)
 (command "stop" "Stop server" servant/stop))

(provide 'servant)

;;; servant.el ends here
