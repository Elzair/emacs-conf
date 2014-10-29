;;; package --- Elzair's Emacs init file
;;; Commentary:
;;; This is my personal start-up file for Emacs.
;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; list of installed packages
(defvar package-list '(ac-geiser ac-sly auto-complete cider clojure-mode clojure-test-mode clojurescript-mode dash evil evil-leader flycheck flycheck-pos-tip geiser goto-chg julia-mode load-theme-buffer-local magit markdown-mode neotree org org-present popup punctuality-logger quack rainbow-delimiters sly smart-tab smartparens solarized-theme tern undo-tree)
  "List of required packages.")

; initialize packages
(package-initialize)

; install missing packages
(let ((package-contents-refreshed nil))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (progn
        (if (not package-contents-refreshed)
            (progn
              (package-refresh-contents)
              (setq package-contents-refreshed t)))
        (package-install package)))))

; evaluate real init file
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/real-init.el")))

(provide 'init)
;;; init ends here
