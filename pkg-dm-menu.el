;;; pkg-dm-menu.el --- Package menu extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh

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
;; Package menu extensions
;;

;;; Code:

(require 'cl-lib)

(require 'elenv)
(require 'msgu)
(require 'prt)
(require 'recentf-excl)
(require 's)

(defcustom pkg-dm-elpa-temp-dir
  (concat user-emacs-directory "elpa/.temp/")
  "Temporary directory to mark packages so it can be deleted afterward."
  :type 'string
  :group 'pkg-dm)

(defcustom pkg-dm-use-doctor nil
  "If non-nil, we check for unmatched dependency."
  :type 'boolean
  :group 'pkg-dm)

(defcustom pkg-dm-no-upgrades nil
  "List of packages will be ignored from the upgrade list."
  :type 'list
  :group 'pkg-dm)

;;
;; (@* "Externals" )
;;

(defvar auto-read-only-file-regexps)

(defvar pkg-dm-package-list)

;;
;; (@* "Util" )
;;

(defun pkg-dm--move-path (path dest)
  "Move PATH to DEST."
  (ignore-errors (make-directory dest t))
  (elenv-shell-execute (if elenv-windows "move" "mv")
                       (unless elenv-windows "-f")
                       (expand-file-name path) (expand-file-name dest)))

(defun pkg-dm--package-dependency (pkg)
  "Return list of dependency from a PKG."
  (let (result (deps (pkg-dm--get-reqs pkg)) dep-name)
    (dolist (dep deps)
      (setq dep-name (car dep))
      (push dep-name result)
      (nconc result (pkg-dm--package-dependency dep-name)))
    (cl-remove 'emacs result)))

(defun pkg-dm-package-dependency-list (lst)
  "Return full dependency list from LST of package."
  (let (result)
    (dolist (pkg lst)
      (setq result (append result (pkg-dm--package-dependency pkg))))
    (reverse (delete-dups result))))

(defun pkg-dm-unused-packages ()
  "Return a list of unused packages."
  (let* ((installed-pkgs (pkg-dm--get-selected-packages))
         (pkg-install-lst pkg-dm-package-list)
         (deps (pkg-dm-package-dependency-list pkg-install-lst))
         (full-pkgs (delete-dups (append pkg-install-lst deps)))
         unused-lst)
    (dolist (pkg installed-pkgs)
      (unless (memq pkg full-pkgs) (push pkg unused-lst)))
    (cl-remove 'emacs (reverse unused-lst))))

(defun pkg-dm-package-desc (name &optional current)
  "Build package description by NAME."
  (cadr (assq name (if current package-alist package-archive-contents))))

(defun pkg-dm--get-reqs (name)
  "Return requires from package NAME."
  (ignore-errors (package-desc-reqs (pkg-dm-package-desc name t))))

(defun pkg-dm--used-elsewhere-p (name)
  "Return non-nil if NAME is used elsewhere."
  (let ((desc (pkg-dm-package-desc name t)))
    (ignore-errors (package--used-elsewhere-p desc nil 'all))))

;;
;; (@* "Dependency" )
;;

(defun pkg-dm--filter-installed (lst)
  "Remove package from LST if not installed."
  (cl-remove-if-not (lambda (elm) (package-installed-p elm)) lst))

(defun pkg-dm--get-selected-packages ()
  "Return selected packages base on the execution's condition."
  (pkg-dm--filter-installed package-activated-list))

(defun pkg-dm-installed-list ()
  "Return full installed package list, including builtins."
  (let (builtins)
    (setq package-activated-list (pkg-dm--filter-installed package-activated-list))
    (dolist (desc package--builtins) (push (nth 0 desc) builtins))
    (cl-delete-duplicates (append builtins package-activated-list))))

;;;###autoload
(defun pkg-dm-rebuild-dependency-list ()
  "Rebuild dependency graph and save to list."
  (interactive)
  (package-initialize)
  (prt-with "Building dependency graph... "
    (recentf-excl-it
      (let ((new-selected-pkg (pkg-dm--get-selected-packages))
            (installed-list (pkg-dm-installed-list)))
        (dolist (name installed-list)
          (if (package-installed-p name)
              (progn
                (prt-update rt (format " `%s`" name))
                (if (or (pkg-dm--used-elsewhere-p name)
                        (package-built-in-p name))
                    (setq new-selected-pkg (remove name new-selected-pkg))
                  (push name new-selected-pkg)))
            (setq new-selected-pkg (remove name new-selected-pkg))))
        (delete-dups new-selected-pkg)
        (setq new-selected-pkg (sort new-selected-pkg #'string-lessp))
        (if (equal new-selected-pkg package-selected-packages)
            (prt-done rt "No need to update dependency graph")
          (package--save-selected-packages new-selected-pkg)
          (prt-done rt "Done rebuild dependency graph"))))))

(defun pkg-dm--after-operations ()
  "After made some operations in the package menu."
  (if pkg-dm-use-doctor
      (pkg-dm-doctor)
    (pkg-dm-rebuild-dependency-list)))

;;
;; (@* "Deletion" )
;;

(defvar pkg-dm--use-real-delete-p t
  "Flag to check if we are really deleting a package.")

(defun pkg-dm-package-delete (desc &optional dep)
  "Safe way to remove package and it's DEP using PKG-DESC."
  (let ((name (package-desc-name desc))
        (used-elsewhere (package--used-elsewhere-p desc nil 'all)))
    (dolist (tmp-desc used-elsewhere) (pkg-dm-package-delete tmp-desc name))
    (when desc
      (let ((pkg-dm--use-real-delete-p t))
        (when (msgu-silent (package-delete desc))
          (if dep (message "Delete package `%s` that is rely on package `%s`" name dep)
            (message "Package `%s` deleted." name)))))))

(defun pkg-dm--package-delete (fnc &rest args)
  "Execution run around function `package-delete' with FNC and ARGS."
  (let ((desc (nth 0 args)))
    (if pkg-dm--use-real-delete-p
        (if-let ((result (ignore-errors (apply fnc args)))) result
          (when-let* ((pkg-dir (package-desc-dir desc))
                      (name (package-desc-name desc))
                      ((pkg-dm--move-path pkg-dir pkg-dm-elpa-temp-dir)))
            (msgu-unsilent
              (message "[INFO] Package `%s` in used, mark `%s` for later deletion"
                       name (file-name-nondirectory pkg-dir)))))
      (pkg-dm-package-delete desc))))

;;
;; (@* "Remove" )
;;

;;;###autoload
(defun pkg-dm-cleanup ()
  "Clean up for package dependencies."
  (interactive)
  (ignore-errors (delete-directory pkg-dm-elpa-temp-dir t)))

(defun pkg-dm--menu-execute (fnc &rest args)
  "Execution around function `package-menu-execute' with FNC and ARGS."
  (let (pkg-dm--use-real-delete-p)
    (when (apply fnc args) (pkg-dm--after-operations))))

;;;###autoload
(defun pkg-dm-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (if-let ((removable (pkg-dm-unused-packages)))
      (when (y-or-n-p
             (format "Packages to delete: %d (%s), proceed? "
                     (length removable)
                     (mapconcat #'symbol-name removable ", ")))
        (mapc (lambda (p)
                (package-delete (cadr (assq p package-alist)) t))
              removable)
        (pkg-dm--after-operations))
    (message "Nothing to autoremove")))

;;
;; (@* "Upgrade" )
;;

(defun pkg-dm--show-upgrades ()
  "Show upgradable packages in one menu."
  (advice-remove 'package-menu--mark-upgrades-1 #'pkg-dm--show-upgrades)
  (when (ignore-errors (package-menu-filter-upgradable))
    (package-menu-mark-upgrades)
    (msgu-current "\n\nPress `x` to execute command; press `u` to unmark packages")))

;;;###autoload
(defun pkg-dm-upgrade-all ()
  "Upgrade for archive packages."
  (interactive)
  (package-menu-mark-upgrades)
  (if package-menu--mark-upgrades-pending
      (advice-add 'package-menu--mark-upgrades-1 :after #'pkg-dm--show-upgrades)
    (pkg-dm--show-upgrades)))

(defun pkg-dm--package-install (fnc &rest args)
  "Advice around execute `package-install' command with FNC and ARGS."
  (let (auto-read-only-file-regexps) (recentf-excl-it (apply fnc args))))

(defun pkg-dm--find-upgrades (fnc &rest args)
  "Advice around execute `package-menu--find-upgrades' command with FNC and ARGS."
  (let ((upgrades (apply fnc args)))
    (seq-filter (lambda (item)
                  (not (memq (car item) pkg-dm-no-upgrades)))
                upgrades)))

;;
;; (@* "Entry" )
;;

(defun pkg-dm-mode--enable ()
  "Enable `pkg-dm-mode'."
  (pkg-dm-cleanup)
  (advice-add 'package-menu-execute :around #'pkg-dm--menu-execute)
  (advice-add 'package-delete :around #'pkg-dm--package-delete)
  (advice-add 'package-install :around #'pkg-dm--package-install)
  (advice-add 'package-install-from-buffer :around #'pkg-dm--package-install)
  (advice-add 'package-menu--find-upgrades :around #'pkg-dm--find-upgrades))

(defun pkg-dm-mode--disable ()
  "Disable `pkg-dm-mode'."
  (advice-remove 'package-menu-execute #'pkg-dm--menu-execute)
  (advice-remove 'package-delete #'pkg-dm--package-delete)
  (advice-remove 'package-install #'pkg-dm--package-install)
  (advice-remove 'package-install-from-buffer #'pkg-dm--package-install)
  (advice-remove 'package-menu--find-upgrades #'pkg-dm--find-upgrades))

;;;###autoload
(define-minor-mode pkg-dm-mode
  "Minor mode `pkg-dm-mode'."
  :group 'pkg-dm
  :global t
  :lighter nil
  (if pkg-dm-mode (pkg-dm-mode--enable) (pkg-dm-mode--disable)))

;;
;; (@* "Core" )
;;

;;;###autoload
(defun pkg-dm-doctor ()
  "Check for matching package list."
  (interactive)
  (pkg-dm-rebuild-dependency-list)
  ;; check for package that shouldn't be installed
  (dolist (name package-selected-packages)
    (unless (memq name pkg-dm-package-list)
      (message "[?] Missing `%s` package, add it to `pkg-dm-package-list' or remove it" name)))
  ;; check dependency or built-in
  (dolist (name pkg-dm-package-list)
    (unless (memq name package-selected-packages)
      (message "[?] The package `%s` can be removed from the `pkg-dm-package-list'" name))))

(provide 'pkg-dm-menu)
;;; pkg-dm-menu.el ends here
