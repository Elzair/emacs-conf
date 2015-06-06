;;; org-funcs.el --- My Org-Mode Functions

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: org latex
;; URL: http://gitlab.com/elzair/project

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

;; My helper functions for Org Mode.

;;; Code:

(require 'org)

(defun org-funcs-preview ()
    "Preview all latex fragments in org buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (not (thing-at-point 'whitespace))
        (open-line 1))
      (org-preview-latex-fragment)))

(provide 'org-funcs)
;;; org-funcs.el ends here
