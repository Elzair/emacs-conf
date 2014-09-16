;;; package --- Elzair's real Emacs init file
;;; Commentary:
;;; This is my real personal start-up file for Emacs.
;;; Code:

; set load path
(add-to-list 'load-path "~/.emacs.d/scripts/")

; require dependencies
(require 'ert)
;(require 'follow-mouse)
(require 'evil)
(require 'evil-leader)
(require 'flycheck)
(require 'key-chord)
(require 'frame-cmds)
(require 'load-theme-buffer-local)
(require 'neotree)
(require 'rainbow-delimiters)
(require 'smart-tab)
(require 'tern)

; enable and configure vi emulation
(global-evil-leader-mode)
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)

; vim-style keybindings
(key-chord-define evil-insert-state-map "ii" 'evil-normal-state)
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
(set-default-font "InconsolataLGC-12")
(load-theme 'solarized-dark t)

; enable/disable certain features
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

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

; create function for "smart" indenting on lisp repls
(defun eval-or-newline-and-indent (repl-newline-and-indent repl-eval)
  "This function should evaluate an s-expression or create a newline.
REPL-NEWLINE-AND-INDENT is the repls's equivalent to 'newline-and-indent.
REPL-EVAL is the repl's function to evaluate an expression."
  (interactive)
  (if (eolp) (funcall repl-eval) (funcall repl-newline-and-indent)))

; configure SLIME
(cond
 ((string-match "darwin" system-configuration)
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))
 ((string-match "linux" system-configuration)
  (setq inferior-lisp-program "/usr/bin/sbcl")))
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf slime-banner))
(defun my-slime-mode-hook ()
  "My slime-mode-hook."
  (set-up-slime-ac)                   ; set-up SLIME autocomplete
  (rainbow-delimiters-mode))

; configure common lisp mode
(defun my-common-lisp-mode-hook ()
  "My cl-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode))

; configure scheme repl
(setq scheme-program-name "guile")
(defun my-inferior-scheme-mode-hook ()
  "My ism-repl-mode-hook."
  (require 'quack)
  (setq quack-fontify-style 'emacs)
  (rainbow-delimiters-mode))

; configure geiser
(defun my-geiser-repl-mode-hook ()
  "My geiser-repl-mode-hook."
  (require 'quack)
  (setq geiser-active-implementations 'guile)
  (setq quack-fontify-style 'emacs)
  (rainbow-delimiters-mode)
  (setq geiser-repl-query-on-kill-p nil)
  (setq geiser-repl-use-other-window nil)
  (define-key
    evil-insert-state-local-map
    "\C-m"
    (lambda ()
      (interactive)
      (eval-or-newline-and-indent
       'geiser-repl--newline-and-indent
       'geiser-repl--maybe-send)))
  (define-key
    evil-insert-state-local-map
    [return]
    (lambda ()
      (interactive)
      (eval-or-newline-and-indent
       'geiser-repl--newline-and-indent
       'geiser-repl--maybe-send))))

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
  (define-key
    evil-insert-state-local-map
    "\C-m"
    (lambda ()
      (interactive)
      (eval-or-newline-and-indent
       'newline-and-indent
       'ielm-return)))
  (define-key
    evil-insert-state-local-map
    [return]
    (lambda ()
      (interactive)
      (eval-or-newline-and-indent
       'newline-and-indent
       'ielm-return))))

; configure scheme files
(defun my-scheme-mode-hook ()
  "My scheme-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode))

; configure geiser
(defun my-geiser-mode-hook ()
  "My geiser-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode)
  (ac-geiser-setup))

; configure javascript files
(defun my-js2-mode-hook ()
  "My javascript-mode-hook."
  (linum-mode)
  (tern-mode t))

(defun my-emacs-lisp-mode-hook ()
  "My elisp-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode))

(defun my-clojure-mode-hook ()
  "My clojure-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode))

(defun my-clojurescript-mode-hook ()
  "My clojurescript-mode-hook."
  (linum-mode)
  (rainbow-delimiters-mode))

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

(add-hook 'slime-mode-hook 'my-slime-mode-hook)
(add-hook 'slime-repl-mode-hook 'my-slime-mode-hook)
(add-hook 'geiser-repl-mode-hook 'my-geiser-repl-mode-hook)
(add-hook 'lisp-mode-hook 'my-common-lisp-mode-hook)
(add-hook 'inferior-scheme-mode-hook 'my-inferior-scheme-mode-hook)
(add-hook 'geiser-repl-mode-hook 'my-geiser-repl-mode-hook)
(add-hook 'ielm-mode-hook 'my-ielm-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'geiser-mode-hook 'my-geiser-mode-hook)
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(add-hook 'clojurescript-mode-hook 'my-clojurescript-mode-hook)
(add-hook 'neotree-mode-hook 'my-neotree-mode-hook)

; set up autocomplete modes
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes '(slime-repl-mode geiser-repl-mode)))

(require 'smartparens-config)
(smartparens-global-mode t)

; set indentation options
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

; enable autocompletion popup
(require 'auto-complete-config)
(ac-config-default)

; set directory to save backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(neotree)

; fullscreen mode
(defun toggle-full-screen ()
  "This function will toggle fullscreen mode."
  (interactive)
  (cond
   ((string-match "darwin" system-configuration)
    (toggle-frame-fullscreen))
   ((string-match "linux" system-configuration)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))))

; map F11 to fullscreen
(global-set-key [f11] 'toggle-full-screen)

; enable fullscreen by default on OSX
(cond
 ((string-match "darwin" system-configuration)
  (toggle-full-screen)))

(provide 'real-init)
;;; real-init ends here
