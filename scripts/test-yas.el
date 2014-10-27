;;; test-yas --- My test functions for YASnippet.
;;; Commentary:
;;; My test of single-character auto-expansions for YASnippet.
;;; Code:

(require 'yasnippet)

(defmacro yas-expand-snippet-by-name (name mode)
  "Expand the given snippet in the current major mode.

NAME is the name of the snippet.

MODE is the major mode wherein the snippet is defined."
  `(yas-expand-snippet (aref (yas--get-template-by-uuid ',mode ,name)
                             2)))

(defmacro yas--auto-expand-on-keypress (key mode)
  "Set a snippet to automatically expand when its key is pressed.

KEY is both the name of the snippet and the name of its corresponding key.

MODE is the name of the given MODE."
  (interactive)
  `(define-key yas-minor-mode-map
     ,(kbd key)
     (lambda ()
       (interactive)
       (yas-expand-snippet-by-name ,key ,mode))))

(provide 'test-yas)
;;; test-yas.el ends here
