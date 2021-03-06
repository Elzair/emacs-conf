;;; package --- evil-repl
;;; Commentary:
;;; This package provides functionality to work with repls using evil-mode.
;;; Code:

(require 'evil)

(defmacro evil-repl-eval (repl-eval)
  "This macro will expand to a lambda function that will evalulate the given code by calling the REPL-EVAL function."
         `(lambda ()
            (interactive)
            (progn
              (,repl-eval)
              (evil-insert 0 0))))

(defmacro evil-repl-smart (repl-newline-and-indent repl-eval)
  "This macro should evaluate an s-expression or create a newline.
REPL-NEWLINE-AND-INDENT is the repls's equivalent to 'newline-and-indent.
REPL-EVAL is the repl's function to evaluate an expression."
  `(progn
    (define-key evil-insert-state-local-map
      [return]
      ',repl-newline-and-indent)
    (define-key evil-normal-state-local-map
      [return]
      (evil-repl-eval ,repl-eval))))

(provide 'evil-repl)
;;; evil-repl ends here
