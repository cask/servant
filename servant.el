;;; servant.el --- Serve ELPA packages

;; Copyright (C) 2012 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: elpa, server
;; URL: http://github.com/rejeep/servant.el
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.11.0") (commander "0.5.0") (elnode "0.9.9.7.6"))

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
(require 'dash)
(require 'elnode)
(require 'commander)

(defun servant/help ()
  (commander-print-usage-and-exit))

(commander
 (name "servant")
 (description "Serve ELPA packages")

 (default servant/help)

 (option "-h, --help" "Print usage information" servant/help)

 (command "help" "Print usage information" servant/help))

(provide 'servant)

;;; servant.el ends here
