;;; package --- Elzair's Emacs init file
;;; Commentary:
;;; This is my personal start-up file for Emacs.
;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

; list of installed packages
(setq package-list '(ac-geiser ac-slime auto-complete cider clojure-mode clojure-test-mode clojurescript-mode dash evil evil-leader flycheck flycheck-pos-tip geiser goto-chg load-theme-buffer-local neotree org org-present popup quack rainbow-delimiters slime slime-scratch smart-tab smartparens solarized-theme tern undo-tree))

; initialize packages
(package-initialize)

; fetch list of available packages
(unless package-archive-contents 
  (package-refresh-contents))

; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; evaluate real init file
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/real-init.el")))

(provide 'init)
;;; init ends here
