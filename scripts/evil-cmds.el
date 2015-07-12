;;; evil-cmds.el --- Miscellaneous commands for evil-mode

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: evil yank kill
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

;; Currently, this file contains functions to improve copy-paste.

;;; Code:

(require 'neotree)

(defvar evil-cmds-neotree-pushed-dir nil
  "The neotree directory before `evil-cmds-edit-conf' was invoked.")

(defmacro with-beg-and-end (&rest body)
  "This macro defines the beginning and end of a region for the BODY code."
  `(let ((beg (if (> (point) (mark))
                  (mark)
                  (point)))
         (end (if (>= (point) (mark))
                  (point)
                  (mark))))
     ,@body))

(defun evil-cmds-copy ()
  "Copy highlighted text to clipboard."
  (interactive)
  (with-beg-and-end
   (copy-region-as-kill beg end t)))

(defun evil-cmds-cut ()
    "Remove highlighted region and add it to `kill-ring' and clipboard."
    (interactive)
    (with-beg-and-end
     (copy-region-as-kill beg end t)
     (delete-region beg end)))

(defun evil-cmds-edit-conf ()
  "My function for quickly editing Emacs config."
  (interactive)
  (setf evil-cmds-neotree-pushed-dir (file-name-directory (buffer-file-name)))
  (neotree-dir user-emacs-directory)
  (neo-open-file (concat user-emacs-directory "real-init.el")))

(defun evil-cmds-save-conf ()
  "My function for saving Emacs config."
  (interactive)
  (if (not (null evil-cmds-neotree-pushed-dir))
      (neotree-dir evil-cmds-neotree-pushed-dir))
  (setq evil-cmds-neotree-pushed-dir nil)
  (kill-buffer "real-init.el")
  (shell-command "cd ~/.emacs.d && git add -A . && git commit -m 'Updated emacs config' && git pull && git push"))

(provide 'evil-cmds)
;;; evil-cmds.el ends here
