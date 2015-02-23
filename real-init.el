;;; real-init.el --- Elzair's real Emacs init file
;;; Commentary:
;;; This is my real personal start-up file for Emacs.
;;; Code:

; set load path
(add-to-list 'load-path "~/.emacs.d/scripts/")
(add-to-list 'load-path "~/.emacs.d/gnu-apl-mode/")

(package-initialize)

; require dependencies
(require 'common-hooks)
(require 'ert)
(require 'evil)
(require 'evil-leader)
(require 'evil-repl)
(require 'evil-indent)
(require 'flycheck)
(require 'gnu-apl-mode)
(require 'key-chord)
(require 'frame-cmds)
(require 'load-theme-buffer-local)
(require 'neotree)
(require 'orientation)
(require 'presentation)
(require 'punctuality-logger)
(require 'rainbow-delimiters)
(require 'tern)
(require 'yasnippet)

; enable and configure vi emulation
(global-evil-leader-mode)
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)

; vim-style keybindings
(evil-leader/set-leader "-")
(evil-leader/set-key
  "e v" (lambda ()
          (interactive)
          (if (and (boundp 'emacs-screen-orientation)
                   (equal emacs-screen-orientation "vertical"))
              (evil-window-new 40 "~/.emacs.d/real-init.el")
            (evil-window-vnew 80 "~/.emacs.d/real-init.el")))
  "p v" (lambda ()
          (interactive)
          (if (and (boundp 'emacs-screen-orientation)
                   (equal emacs-screen-orientation "vertical"))
              (evil-window-new 40 "~/.emacs.d/init.el")
            (evil-window-vnew 80 "~/.emacs.d/init.el")))
  "s v" (lambda ()
          (interactive)
          (shell-command "cd ~/.emacs.d && git add -A . && git commit -m 'Updated emacs config' && git pull && git push")))
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "ii" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jj" (lambda ()
                                               (interactive)
                                               (right-char 1)))
(evil-ex-define-cmd "er" 'eval-region)
(evil-ex-define-cmd "ev" 'eval-expression)
(evil-ex-define-cmd "hk" 'describe-key)
(evil-ex-define-cmd "hf" 'describe-function)
(evil-ex-define-cmd "hv" 'describe-variable)
(evil-ex-define-cmd "ms" 'magit-status)
(evil-ex-define-cmd "mc" 'magit-commit)
(evil-ex-define-cmd "lb" 'list-buffers)
(evil-ex-define-cmd "sb" 'switch-to-buffer)
(evil-ex-define-cmd "ex" 'execute-extended-command)
(evil-ex-define-cmd "ms" 'magit-status)
(define-minor-mode geiser-evil-mode
  "Geiser-Evil mode."
  :keymap (make-sparse-keymap))
(evil-set-initial-state 'org-present-mode 'emacs)

; set default font and theme
(set-default-font "Inconsolata LGC")
(load-theme 'solarized-dark t)

; enable/disable certain features
(setq column-number-mode t)           ; enable column # display in modeline
(setq inhibit-startup-screen t)       ; disable start screen
(scroll-bar-mode -1)                  ; disable scroll bar
(if (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1) ()) ; disable bottom scroll bar
(tool-bar-mode -1)                    ; disable tool bar
(setq initial-scratch-buffer nil)
(setq mouse-autoselect-window t)      ; turn on focus follows mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; scroll one line at a time
(savehist-mode 1) ; persist minibuffer history across sessions
(setq savehist-file "~/.emacs.d/savehist") ; set file to save history
(setq ring-bell-function 'ignore)     ; stop bell
(global-flycheck-mode)                ; enable syntax checking
(global-auto-revert-mode 1)           ; auto-refresh a changed file
(setq auto-revert-verbose nil)
(custom-set-variables '(android-mode-sdk-dir "/opt/android-sdk-linux"))

; enable autocompletion
(global-company-mode)
(setq company-idle-delay 0.1)
(add-to-list 'company-backends
             '(company-c-headers company-irony
                                 company-tern
                                 sly-company))
(company-quickhelp-mode 1)            ; enable documentaion popup

; enable & configure yasnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode)                     ; enable snippets
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

; configure sly
(setq sly-contribs '(sly-autodoc sly-fancy))
(cond ((string-match "darwin" system-configuration)
       (setq inferior-lisp-program "/usr/local/bin/sbcl"))
      ((string-match "linux" system-configuration)
       (setq inferior-lisp-program "/usr/bin/sbcl")))
(defun my-sly-mrepl-mode-hook ()
  "My sly-mrepl-hook."
  (font-lock-mode 1) ; rainbow-delimiters requires font-lock-mode
  (rainbow-delimiters-mode)
  (evil-repl-smart newline-and-indent
                   sly-mrepl-return))

(defun my-sly-mode-hook ()
    "My sly-mode hook."
  (sly-company-mode))

(defun my-geiser-repl-mode-hook ()
  "My geiser-repl-mode-hook."
  (require 'quack)
  (setq geiser-active-implementations 'guile)
  (setq quack-fontify-style 'emacs)
  (rainbow-delimiters-mode)
  (setq geiser-repl-query-on-kill-p nil)
  (setq geiser-repl-use-other-window nil)
  (evil-repl-smart geiser-repl--newline-and-indent
                   geiser-repl--maybe-send))

(defun my-ielm-mode-hook ()
  "My ielm-mode-hook."
  (rainbow-delimiters-mode)
  (evil-repl-smart newline-and-indent ielm-return))

(defun my-lisp-mode-hook ()
  "My lisp-mode-hook."
  (common-lispy-hooks))

(defun my-scheme-mode-hook ()
  "My scheme-mode-hook."
  (common-lispy-hooks))

(defun my-geiser-mode-hook ()
  "My geiser-mode-hook."
  (common-lispy-hooks))

(defun my-javascript-mode-hook ()
  "My javascript-mode-hook."
  (linum-mode)
  (tern-mode t)
  (evil-smart-indent))

(defun my-emacs-lisp-mode-hook ()
  "My elisp-mode-hook."
  (common-lispy-hooks))

(defun my-clojure-mode-hook ()
  "My clojure-mode-hook."
  (common-lispy-hooks))

(defun my-clojurescript-mode-hook ()
  "My clojurescript-mode-hook."
  (common-lispy-hooks))

(defun my-haskell-mode-hook ()
    "My haskell-mode-hook."
  (linum-mode)
  (haskell-indentation-mode))

(defun my-html-mode-hook ()
  "My html-mode hook."
  (linum-mode)
  (evil-smart-indent))

(defun my-rust-mode-hook ()
    "My rust-mode-hook."
    (linum-mode)
    (evil-smart-indent))

<<<<<<< HEAD
(defun my-j-mode-hook ()
    "My j-mode-hook."
    (linum-mode))
=======
(defun my-gnu-apl-mode-hook ()
    "My gnu-apl-mode-hook."
    (linum-mode)
    ())
>>>>>>> 77b97a6713c2e65720ebb480e5e4c6fdb5b52c85

(defun my-eshell-mode-hook ()
    "My eshell-mode-hook."
  (setq eshell-path-env "/opt/android-sdk-linux/build-tools:/opt/android-sdk-linux/platform-tools:/opt/android-sdk-linux/tools:/usr/bin:/usr/local/bin:/bin:/usr/games"))

(defun my-shell-mode-hook ()
  "My shell-mode-hook."
  (linum-mode)
  (evil-smart-indent))

(defun my-org-mode-hook ()
  "My org-mode hook."
  (setq org-src-fontify-natively t))

(defun my-org-present-mode-hook ()
  "My org-present-mode hook."
  (org-present-big)
  (org-display-inline-images))

(defun my-org-present-mode-quit-hook ()
  "My org-present-mode-quit hook."
  (org-present-small)
  (org-remove-inline-images))

(defun my-neotree-mode-hook ()
  "My neotree-mode-hook."
  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
  (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
  (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
  (key-chord-define evil-normal-state-local-map "ma" 'neotree-create-node)
  (key-chord-define evil-normal-state-local-map "md" 'neotree-delete-node)
  (key-chord-define evil-normal-state-local-map "mm" 'neotree-rename-node))

(defun my-image-after-revert-hook ()
  "My image-after-revert-hook to automatically refresh images."
  (progn
    (clear-image-cache)
    (image-toggle-display-image)))

(add-hook 'sly-mrepl-mode-hook 'my-sly-mrepl-mode-hook)
(add-hook 'geiser-repl-mode-hook 'my-geiser-repl-mode-hook)
(add-hook 'ielm-mode-hook 'my-ielm-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'geiser-mode-hook 'my-geiser-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'js-mode-hook 'my-javascript-mode-hook)
(add-hook 'js2-mode-hook 'my-javascript-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(add-hook 'clojurescript-mode-hook 'my-clojurescript-mode-hook)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'html-mode-hook 'my-html-mode-hook)
(add-hook 'rust-mode-hook 'my-rust-mode-hook)
(add-hook 'j-mode-hook 'my-j-mode-hook)
(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-present-mode-hook 'my-org-present-mode-hook)
(add-hook 'org-present-mode-quit-hook 'my-org-present-mode-quit-hook)
(add-hook 'neotree-mode-hook 'my-neotree-mode-hook)
(add-hook 'image-after-revert-hook 'my-image-after-revert-hook)

; set indentation options
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

; set directory to save backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(neotree)

; map F11 to fullscreen
(global-set-key [f11] 'toggle-presentation)

; enable fullscreen by default on OSX
(cond
 ((string-match "darwin" system-configuration)
  (toggle-frame-fullscreen)))

; Prevent annoying \"Active processes exist \" query on quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

; hack to enable focus-follow-mouse on OSX
(cond ((string-match "darwin" system-configuration)
       (progn
         (require 'follow-mouse)
         (turn-on-follow-mouse))))

(provide 'real-init)
;;; real-init.el ends here
