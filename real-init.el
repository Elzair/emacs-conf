;;; package --- Elzair's real Emacs init file
;;; Commentary:
;;; This is my real personal start-up file for Emacs.
;;; Code:

(defun emacs-welcome ()
  "This function will print a welcome message in *scratch*."
  (progn
    (other-window 1)
    (animate-string (concat ";; Welcome to "
                            (substring (emacs-version) 0 16)
                            ".")
                    0 1)))


; set load path
(add-to-list 'load-path "~/.emacs.d/scripts/")

(package-initialize)

; require dependencies
(require 'common-hooks)
(require 'ert)
(require 'evil)
(require 'evil-leader)
(require 'evil-repl)
(require 'flycheck)
(require 'key-chord)
(require 'frame-cmds)
(require 'load-theme-buffer-local)
(require 'neotree)
(require 'presentation)
(require 'rainbow-delimiters)
(require 'smart-tab)
(require 'tern)
(require 'yasnippet)

; enable and configure vi emulation
(global-evil-leader-mode)
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)

; vim-style keybindings
(evil-leader/set-leader "-")
(evil-leader/set-key
  "e v" (lambda () (interactive) (evil-window-vnew 80 "~/.emacs.d/real-init.el"))
  "p v" (lambda () (interactive) (evil-window-vnew 80 "~/.emacs.d/init.el"))
  "s v" (lambda () (interactive) (shell-command "cd ~/.emacs.d && git add -A . && git commit -m 'Updated emacs config' && git pull && git push")))
(key-chord-mode 1)
(evil-ex-define-cmd "er" 'eval-region)
(define-minor-mode geiser-evil-mode
  "Geiser-Evil mode."
  :keymap (make-sparse-keymap))

; set default font and theme
(set-default-font "Inconsolata LGC")
(load-theme 'solarized-dark t)

; enable/disable certain features
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
(global-smart-tab-mode 1)             ; enable smart tabbing
(global-flycheck-mode)
(global-auto-revert-mode 1)           ; auto-refresh a changed file
(setq auto-revert-verbose nil)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode)                     ; enable snippets

; YASnippet keybindings
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

; configure SLIME
(cond
 ((string-match "darwin" system-configuration)
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))
 ((string-match "linux" system-configuration)
  (setq inferior-lisp-program "/usr/bin/sbcl")))
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf slime-banner slime-autodoc))

(defun my-slime-repl-mode-hook ()
  "My slime-mode-hook."
  (set-up-slime-ac)                   ; set-up SLIME autocomplete
  (rainbow-delimiters-mode)
  (evil-repl-smart slime-repl-newline-and-indent
                   slime-repl-return))

; configure geiser
(setq geiser--im)
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
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1)
  (evil-repl-smart newline-and-indent ielm-return))

; configure common lisp mode
(defun my-lisp-mode-hook ()
  "My slime-mode-hook."
  (auto-complete-mode t)
  (set-up-slime-ac)
  (common-lispy-hooks))

; configure scheme files
(defun my-scheme-mode-hook ()
  "My scheme-mode-hook."
  (common-lispy-hooks))

; configure geiser
(defun my-geiser-mode-hook ()
  "My geiser-mode-hook."
  (ac-geiser-setup)
  (common-lispy-hooks))

; configure javascript files
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

; configure org-present
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

(defun my-after-real-init-hook ()
  "My after-real-init-hook."
  (emacs-welcome))

(add-hook 'slime-repl-mode-hook 'my-slime-repl-mode-hook)
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
(add-hook 'org-present-mode-ook 'my-org-present-mode-hook)
(add-hook 'org-present-mode-quit-hook 'my-org-present-mode-quit-hook)
(add-hook 'neotree-mode-hook 'my-neotree-mode-hook)
(add-hook 'image-after-revert-hook 'my-image-after-revert-hook)
(add-hook 'after-real-init-hook 'my-after-real-init-hook)

; set up autocomplete modes
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes '(emacs-lisp-mode
                            ecmascript-mode javascript-mode js-mode js2-mode
                            slime-repl-mode slime-mode lisp-mode
                            scheme-mode geiser-repl-mode)))

(require 'smartparens-config)
(smartparens-global-mode t)

; set indentation options
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

; enable autocompletion popup
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)
  (global-auto-complete-mode t)
  (auto-complete-mode t)


  ; dirty fix for having AC everywhere
  (define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                             (auto-complete-mode 1))))
  (real-global-auto-complete-mode t))

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

(key-chord-define evil-insert-state-map "ii" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jj" (lambda ()
                                               (interactive)
                                               (evil-forward-char 1)))

; hack to enable focus-follow-mouse on OSX
(cond ((string-match "darwin" system-configuration)
       (progn
         (require 'follow-mouse)
         (turn-on-follow-mouse))))

(run-hooks 'after-real-init-hook)

(provide 'real-init)
;;; real-init ends here
