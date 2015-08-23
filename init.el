;;; init.el --- Elzair's Emacs init file
;;; Commentary:
;;; This is my personal start-up file for Emacs.
;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar package-list '(android-mode cider clojure-mode clojurescript-mode company company-c-headers company-irony company-quickhelp company-tern dash dockerfile-mode emmet-mode evil ess flycheck flycheck-irony flycheck-pos-tip geiser glsl-mode goto-chg haskell-mode irony irony-eldoc j-mode julia-mode load-theme-buffer-local magit markdown-mode neotree org org-if org-present ox-reveal php-extras popup punctuality-logger puppet-mode quack racket-mode rainbow-delimiters rust-mode sly sly-company solarized-theme tern toml-mode undo-tree web-mode yaml-mode yasnippet)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android-sdk-linux")
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/" t))))
 '(backup-directory-alist (quote ((".*" . "/tmp/"))))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(j-console-cmd "ijconsole")
 '(magit-use-overlays nil)
 '(org-hide-emphasis-markers t)
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame))))
 '(package-selected-packages
   (quote
    (org-if glsl-mode yasnippet yaml-mode web-mode toml-mode solarized-theme sly-company rust-mode rainbow-delimiters racket-mode quack puppet-mode punctuality-logger php-extras ox-reveal org-present neotree markdown-mode magit load-theme-buffer-local julia-mode j-mode irony-eldoc haskell-mode geiser flycheck-pos-tip flycheck-irony evil-leader ess emmet-mode dockerfile-mode company-tern company-quickhelp company-irony company-c-headers clojurescript-mode cider android-mode)))
 '(punctuality-logger-use-version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; evaluate real init file
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/real-init.el")))

(provide 'init)
;;; init.el ends here
