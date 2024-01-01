;;; pkg-dm.el --- Package dependencies management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/pkg-dm
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (elenv "0.1.0") (msgu "0.1.0") (prt "0.1.0") (recentf-excl "0.1.0") (s "1.12.0"))
;; Keywords: maint

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Package dependencies management
;;

;;; Code:

(require 'package)

(defgroup pkg-dm nil
  "Package dependencies management."
  :prefix "pkg-dm-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/pkg-dm"))

(defcustom pkg-dm-package-list
  '()
  "List of package you wish to install."
  :type 'list
  :group 'pkg-dm)

;;
;; (@* "Externals" )
;;

(declare-function pkg-dm-rebuild-dependency-list "pkg-md-menu.el")

;;
;; (@* "Installation" )
;;

(defun pkg-dm-install (pkg)
  "Install PKG package."
  (unless (package-installed-p pkg)
    (unless package-archive-contents (package-refresh-contents))
    (package-install pkg)))

(defun pkg-dm-ensure-install (packages)
  "Assure every PACKAGES is installed."
  (mapc #'pkg-dm-install packages)
  ;; Rebuild after done the installation
  (when package-archive-contents
    (pkg-dm-rebuild-dependency-list)
    (package-initialize)))

;;;###autoload
(defun pkg-dm-install-all ()
  "Install all needed packages from this configuration."
  (interactive)
  (pkg-dm-ensure-install pkg-dm-package-list))

(provide 'pkg-dm)
;;; pkg-dm.el ends here
