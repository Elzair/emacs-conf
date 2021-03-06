;;; common-hooks --- Common hook functions and macros
;;; Commentary:
;;; This module provides functions and macros for use in many mode-hooks.
;;; Code:

(require 'rainbow-delimiters)
(require 'evil-indent)
(require 'sexp-move)

(defun common-lispy-hooks ()
  "This function is useful for modes to edit Lisp code."
    (linum-mode)
    (rainbow-delimiters-mode)
    (define-key evil-insert-state-local-map
      (kbd "<tab>") 'sexp-move-forward)
    (define-key evil-insert-state-local-map
      (kbd "C-<tab>") 'sexp-move-backward))

(defun common-comint-hooks ()
    "This function provides easier history access in comint modes."
  (define-key evil-insert-state-local-map
      (kbd "<up>") 'comint-previous-input)
  (define-key evil-insert-state-local-map
      (kbd "<down>") 'comint-next-input))

(provide 'common-hooks)
;;; common-hooks.el ends here
