;;; package --- evil-repl
;;; Commentary:
;;; This package provides functionality to work with repls using evil-mode.
;;; Code:

(require 'evil)

(defmacro evil-repl-smart-eval (repl-eval)
  "This macro will expand to a lambda function that will evalulate the given code by calling the REPL-EVAL function."
         `(lambda ()
            (interactive)
            (progn
              (,@repl-eval)
              (evil-insert 0 0))))

(defun evil-repl-smart (repl-newline-and-indent repl-eval)
  "This function should evaluate an s-expression or create a newline.
REPL-NEWLINE-AND-INDENT is the repls's equivalent to 'newline-and-indent.
REPL-EVAL is the repl's function to evaluate an expression."
  (interactive)
  (progn
    (define-key evil-insert-state-map
      [return]
      'repl-newline-and-indent)
    (define-key evil-normal-state-map
      [return]
      (evil-repl-smart-eval repl-eval))))

(provide 'evil-repl)
;;; evil-repl ends here
