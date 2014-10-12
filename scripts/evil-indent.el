;;; evil-indent --- Smart Indentation module for Evil-Mode
;;; Commentary:
;;; This module helps a user correctly indent code in Evil-Mode.
;;; Code:

(require 'evil)

(defun evil-smart-indent ()
  "This function binds the return key to `newline-and-indent' for the current buffer."
    (interactive)
    (define-key evil-insert-state-local-map
      [return]
      'newline-and-indent))

(provide 'evil-indent)
;;; evil-indent.el ends here
