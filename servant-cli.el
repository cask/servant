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
(require 'shut-up)
(require 'web-server)
(require 'eieio)
(require 'cl)

(shut-up-silence-emacs)



(defvar servant-pid-file nil
  "User overwritten PID file path.")

(defvar servant-port 9191
  "Server port.")

(defvar servant-root-path default-directory
  "Run commands with this as root.")

(defvar servant-packages-path nil
  "Path to packages directory.")

(defun servant-path ()
  "Path to main Servant directory."
  (f-expand "servant" servant-root-path))

(defun servant-tmp-path ()
  "Path to tmp directory."
  (f-expand "tmp" (servant-path)))

(defun servant-packages-path ()
  "Path to package directory."
  (or servant-packages-path (f-expand "packages" (servant-path))))

(defun servant-index-file ()
  "Path to index (archive content) file."
  (f-expand "archive-contents" (servant-packages-path)))

(defun servant-pid-file ()
  "Path to server PID file."
  (or servant-pid-file (f-expand "servant.pid" (servant-tmp-path))))


;;;; Options

(defun servant/pid (pid-file)
  "Set path to PID file to PID-FILE.

Default is servant/tmp/servant.pid."
  (setq servant-pid-file pid-file))

(defun servant/port (port)
  "Set server PORT, defaulting to 9191."
  (setq servant-port (string-to-number port)))

(defun servant/path (path)
  "Set PATH as root path when running command."
  (setq servant-root-path path))

(defun servant/packages-path (packages-path)
  "Set path to packages directory to PACKAGES-PATH.

If PACKAGES-PATH is relative, it will be relative
  `default-directory' or the `--path' option is specified."
  (setq servant-packages-path
        (if (f-absolute? packages-path)
            packages-path
          (f-expand packages-path servant-root-path))))

(defun servant/debug ()
  "Enable debug information."
  (setq debug-on-error t))


;;;; Commands

(defun servant/help ()
  "Show Servant usage information."
  (commander-print-usage-and-exit))

(defun servant/init ()
  "Initialize the project for Servant."
  (when (f-dir? (servant-path))
    (error (ansi-red "Directory `servant` already exists.")))
  (f-mkdir (servant-path))
  (f-mkdir (servant-tmp-path))
  (f-mkdir (servant-packages-path))
  (message "create %s" (ansi-green "servant"))
  (message "create %s" (ansi-green "servant/tmp"))
  (message "create %s" (ansi-green "servant/packages")))

(defun servant/start ()
  "Start server."
  (servant-start)
  (while t
    (sit-for 1)))

(defun servant/stop ()
  "Stop server."
  (servant-stop))

(defun servant/index ()
  "Generate index (archive-contents) file for all packages."
  (f-write (servant--create-index-string (servant-packages-path))
           'utf-8 (servant-index-file)))


;;;; Commander schema

(commander
 (name "servant")
 (description "Serve ELPA packages")
 (config ".servant")

 (default servant/help)

 (option "-h, --help" servant/help)
 (option "-p <port>, --port <port>" servant/port)
 (option "-P <file>, --pid <file>" servant/pid)
 (option "--debug" servant/debug)
 (option "--index" servant/index)
 (option "--path <path>" servant/path)
 (option "--packages-path <packages-path>" servant/packages-path)

 (command "init" servant/init)
 (command "index" servant/index)
 (command "help" servant/help)
 (command "start" servant/start)
 (command "stop" servant/stop))

(provide 'servant-cli)

;;; servant-cli.el ends here
