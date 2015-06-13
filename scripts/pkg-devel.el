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

(setf lexical-binding t)

(require 'package)

(defun pkg-devel-get-installed-packages ()
  "This function retrieves a list of all installed elisp packages."
  (mapcar #'(lambda (pkg)
              (car pkg))
          (remove-if-not #'(lambda (pkg)
                             (equal "installed"
                                    (package-desc-status (car pkg))))
                         (package-menu--refresh nil))))

(defun pkg-devel-get-package-desc (name version)
  "This function outputs `package-desc' struct containing information on the package specified by NAME and VERSION."
  (let ((pkg-name (concat name "-" version)))
    (car (remove-if-not #'(lambda (pkg)
                            (equal pkg-name (package-desc-full-name pkg)))
                        (pkg-devel-get-installed-packages)))))

(defun pkg-devel-create-archive (name version &optional path)
    "Create a .tar archive of the package specified by NAME, VERSION and, optionally, PATH."
    (when (null path)
      (setf path "."))
    (directory-files path t "^[^.]"))

(provide 'pkg-devel)
;;; pkg-devel.el ends here
