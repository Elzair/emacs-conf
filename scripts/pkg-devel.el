;; -*- lexical-binding: t -*-
;;; pkg-devel.el --- Functions for elisp package development

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: package pkg devel
;; URL: https://gitlab.com/elzair/pkg-devel

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

(require 'cl)
(require 'package)

(defun pkg-devel-read-file (path)
  "Returns the content of file indicated by PATH."
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

(defun pkg-devel-get-version (pkg-name elisp-files path)
  "Get the version number for the package PKG-NAME with the ELISP-FILES at the given PATH."
  (let* ((mult-file (remove-if-not #'(lambda (f)
                                       (string-equal (concat pkg-name
                                                             "-pkg.el")
                                                     (file-name-nondirectory f)))
                                   elisp-files))
         (pkg-file  (expand-file-name (car mult-file) path))
         (contents  (pkg-devel-read-file pkg-file))
         (sexp      (car (read-from-string contents)))
         (version   (nth 2 sexp)))
    version))

(defun pkg-devel-create-archive (name &optional path)
  "Create a .tar archive of the package specified by NAME and, optionally, PATH."
  (let* ((path-def  (if (stringp path)
                        path
                        default-directory))
         (dir       (file-name-as-directory (expand-file-name name path-def)))
         (files     (directory-files dir t "^[^.]"))
         (rel-files (mapcar #'(lambda (f) (file-relative-name f path-def))
                            files))
         (el-files  (remove-if-not #'(lambda (f)
                                       (string-equal "el"
                                                     (file-name-extension f)))
                                   rel-files))
         
         (version   (pkg-devel-get-version name el-files path-def))
         (command   (concat "tar -cf "
                            name
                            "-"
                            version
                            ".tar "
                            (mapconcat 'identity
                                       rel-files
                                       " "))))
    command))

(provide 'pkg-devel)
;;; pkg-devel.el ends here
