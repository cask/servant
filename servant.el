;;; servant.el --- Serve ELPA packages

;; Copyright (C) 2012 Johan Andersson

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
(require 'commander)

(defvar servant-port 9191)

(defconst servant-path
  (f-expand "servant"))

(defconst servant-packages-path
  (f-expand "packages" servant-path))

(defconst servant-index-file
  (f-expand "index" servant-path))

(defvar servant-pid-file
  (f-expand "servant.pid" servant-path))

(defun servant/pid (pid)
  (setq servant-pid-file pid))

(defun servant/port (port)
  (setq servant-port port))

(defun servant/help ()
  (commander-print-usage-and-exit))

(defun servant/init ()
  (when (f-dir? servant-path)
    (error (ansi-red "Directory `servant` already exists."))
  (f-mkdir servant-path)
  (f-mkdir servant-packages-path)
  (message "create %s" (ansi-green "servant"))
  (message "create %s" (ansi-green "servant/packages")))

(defun servant/start ()
  (unless (f-dir? servant-path)
    (error (ansi-red "Servant not initialized, run `servant init`.")))
  (unless (f-dir? servant-index-file)
    (error (ansi-red "No index, run `servant index` to create")))

  (message "starting server on port %s using PID %s" servant-port servant-pid-file)
  )

(defun servant/index ()
  (message "Indexing... %s" servant-index-file)
  )

(commander
 (name "servant")
 (description "Serve ELPA packages")

 (default servant/help)

 (option "-h, --help" "Print usage information" servant/help)
 (option "-p <port>, --port <port>" "Use port (default: 9191)" servant/port)
 (option "-P <file>, --pid <file>" "File to store PID (default: servant.pid)" servant/pid)

 (command "init" "Initialize servant" servant/init)
 (command "index" "Build package index" servant/index)
 (command "help" "Print usage information" servant/help)
 (command "start" "Start server" servant/start))

(provide 'servant)

;;; servant.el ends here
