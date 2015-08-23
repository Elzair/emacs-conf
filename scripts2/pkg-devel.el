;; -*- lexical-binding: t -*-
;;; pkg-devel.el --- Functions for elisp package development

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: package pkg devel

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for developing Emacs Lisp packages.

;;; Code:

(require 'cl-lib)
(require 'package)

(defun pkg-devel-read-file (path)
  "Returns as a string the contents of file indicated by PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pkg-devel-get-installed-packages ()
  "This function retrieves a list of all installed elisp packages."
  (mapcar #'(lambda (pkg)
              (car pkg))
          (remove-if-not #'(lambda (pkg)
                             (string-equal "installed"
                                           (package-desc-status (car pkg))))
                         (package-menu--refresh nil))))

(defun pkg-devel-get-package-desc (name &optional version)
  "This function outputs `package-desc' struct containing information on the package specified by NAME and VERSION."
  (let ((pkg-name (if (stringp version)
                      (concat name "-" version)
                      name)))
    (remove-if-not #'(lambda (pkg)
                       (eq 0 (string-match pkg-name
                                           (package-desc-full-name pkg))))
                   (pkg-devel-get-installed-packages))))

(defun pkg-devel-get-version (name path)
  "Get the version number for the package NAME at the given PATH."
  (let* ((dir       (concat path name))
         (files     (directory-files dir t "^[^.]"))
         (el-files  (remove-if-not #'(lambda (f)
                                       (string-equal "el"
                                                     (file-name-extension f)))
                                   files))
         (mult-file (remove-if-not #'(lambda (f)
                                       (string-equal (concat name
                                                             "-pkg.el")
                                                     (file-name-nondirectory f)))
                                   el-files))
         (pkg-file  (expand-file-name (car mult-file) path))
         (contents  (pkg-devel-read-file pkg-file))
         (sexp      (car (read-from-string contents)))
         (version   (nth 2 sexp)))
    version))

(defun pkg-devel-create-archive (name version path)
  "Create a tar archive of the package specified by NAME, VERSION and PATH.
Returns the file path to the new archive."
  (let* ((dir       (concat path name))
         (files     (directory-files dir t ".el$"))
         (new-name  (concat name "-" version))
         (new-dir   (concat path new-name))
         (new-files (mapcar #'(lambda (f)
                                (concat (file-name-as-directory new-dir)
                                        (file-name-nondirectory f)))
                            files))
         (zip-files (mapcar* #'cons files new-files))
         (rel-files (mapcar #'(lambda (f) (file-relative-name f path))
                            new-files)))
    ; Create directory name-version and copy files into it
    (make-directory new-dir)
    (mapc #'(lambda (z)
              (copy-file (car z) (cdr z) t))
          zip-files)
    ; Use tar to create archive from new directory
    (let ((command   (concat "cd "
                             path
                             "; tar -cf "
                             new-name
                             ".tar "
                             (mapconcat 'identity
                                        rel-files
                                        " "))))
      (shell-command command))
    ; Delete new directory
    (delete-directory new-dir t)
    (concat new-dir ".tar")))

(defun pkg-devel-refresh (pkg-dir)
    "Reinstall the package whose name is the directory of the current buffer: PKG-DIR."
    (interactive "DPackage Directory: ")
    (let* ((pkg-dir-as-file (directory-file-name pkg-dir))
           (name            (file-name-nondirectory pkg-dir-as-file))
           (path            (file-name-directory    pkg-dir-as-file))
           (version         (pkg-devel-get-version  name path))
           (old-versions    (pkg-devel-get-package-desc name version)))
      (mapc #'package-delete old-versions)
      (let ((archive-file (pkg-devel-create-archive name version path)))
        (package-install-file archive-file)
        (delete-file archive-file))))

(provide 'pkg-devel)
;;; pkg-devel.el ends here
