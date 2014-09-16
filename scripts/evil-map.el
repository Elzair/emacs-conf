;;; package --- vim-style map for evil-mode
;;; Commentary:
;;; This package duplicates the functionality of map, nmap, imap, vmap, etc. in vim.
;;; Code:
(require 'evil)

(defun evil-map (keys macro)
  "This function, whenever the user enters the KEYS, will execute the keyboard MACRO in normal state."
  (define-key evil-normal-state-map keys (lambda (macro)
                                           (interactive)
                                           (execute-kbd-macro macro))))

(defun evil-imap (keys macro)
  "This function, whenever the user enters the KEYS, will execute the keyboard MACRO in insert state."
  (define-key evil-insert-state-map keys (lambda (macro)
                                           (interactive)
                                           (execute-kbd-macro macro))))

(defun evil-vmap (keys macro)
  "This function, whenever the user enters the KEYS, will execute the keyboard MACRO in visual state."
  (define-key evil-visual-state-map keys (lambda (macro)
                                           (interactive)
                                           (execute-kbd-macro macro))))

(provide 'evil-map)
;;; evil-map.el ends here
