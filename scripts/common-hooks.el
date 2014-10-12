;;; common-hooks --- Common hook functions and macros
;;; Commentary:
;;; This module provides functions and macros for use in many mode-hooks.
;;; Code:

(require 'rainbow-delimiters)
(require 'evil-indent)

(defun common-lispy-hooks ()
  "This function is useful for modes to edit Lisp code."
    (linum-mode)
    (rainbow-delimiters-mode)
    (evil-smart-indent))

(provide 'common-hooks)
;;; common-hooks.el ends here
