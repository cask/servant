;;; servant-cli.el --- Servant: CLI frontend         -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <lunaryorn@gmail.com>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Command line interface for Servant.

;;; Code:

(require 'f)
(require 'servant (f-join (f-parent (f-this-file)) "servant"))

(require 'commander)
(require 'ansi)


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
  "Set path to PID file to PID-FILE."
  (setq servant-pid-file pid-file))

(defun servant/port (port)
  "Set server port to PORT."
  (setq servant-port port))

(defun servant/debug ()
  "Enable debug options."
  (setq debug-on-error t
        debug-on-entry t))


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

(provide 'servant-cli)

;;; servant-cli.el ends here
