;;; init.el --- Elzair's Emacs init file
;;; Commentary:
;;; This is my personal start-up file for Emacs.
;;; Code:
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar package-list '(cider clojure-mode clojure-test-mode clojurescript-mode company company-c-headers company-irony company-tern dash dockerfile-mode emmet-mode evil evil-leader flycheck flycheck-pos-tip geiser goto-chg haskell-mode julia-mode load-theme-buffer-local magit markdown-mode neotree org org-present php-extras popup punctuality-logger quack rainbow-delimiters restclient sly sly-company solarized-theme tern toml-mode undo-tree web-mode yaml-mode yasnippet)
  "List of installed packages.")

(package-initialize) ;; Initialize packages

(defun check-packages-and-install-missing-packages ()
  "Ensure that each package in `package-list' is installed.
If a packages is not installed, refresh the package contents
and install the package."
  (interactive)
  (let ((package-contents-refreshed nil))
    (dolist (package package-list)
      (unless (package-installed-p package)
        (when (not package-contents-refreshed)
          (package-refresh-contents)
          (setq package-contents-refreshed t))
        (package-install package)))))

(check-packages-and-install-missing-packages)

; evaluate real init file
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/real-init.el")))

(provide 'init)
;;; init.el ends here
