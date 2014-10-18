;;; slime-config --- my SLIME configuration
;;; Commentary:
;;; This file contains code to configure SLIME and dynamically
;;; reload it without stopping Emacs.
;;; Code:

(require 'slime)
(require 'slime-asdf)
(require 'slime-autodoc)
(require 'slime-autoloads)
(require 'slime-banner)
(require 'slime-fancy)

(defun load-slime ()
  "This function configures SLIME."
  (cond
   ((string-match "darwin" system-configuration)
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))
   ((string-match "linux" system-configuration)
    (setq inferior-lisp-program "/usr/bin/sbcl")))
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-autodoc)))

(defun slime-reload ()
  "This function dynamically reloads SLIME."
  (interactive)
  (mapc 'load-library
        (reverse (remove-if-not
                  (lambda (feature) (string-prefix-p "slime" feature))
                  (mapcar 'symbol-name features))))
  (setq slime-protocol-version (slime-changelog-date))
  (load-slime))

(provide 'slime-config)
;;; slime-config.el ends here
