;;; init.el --- Elzair's Emacs init file
;;; Commentary:
;;; This is my personal start-up file for Emacs.
;;; Code:
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar package-list '(android-mode cider clojure-mode clojure-test-mode clojurescript-mode company company-c-headers company-irony company-quickhelp company-tern dash dockerfile-mode emmet-mode evil evil-leader flycheck flycheck-pos-tip geiser goto-chg haskell-mode julia-mode load-theme-buffer-local magit markdown-mode neotree org org-present ox-reveal php-extras popup punctuality-logger puppet-mode quack rainbow-delimiters rust-mode sly sly-company solarized-theme tern toml-mode undo-tree web-mode yaml-mode yasnippet)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android-sdk-linux")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(magit-use-overlays nil)
 '(punctuality-logger-use-version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
