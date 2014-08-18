; set package list
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; set load path
(add-to-list 'load-path "~/.emacs.d/scripts")


; initialize packages
(package-initialize)

; require dependencies
(require 'follow-mouse)
(require 'evil)
(require 'key-chord)
(require 'frame-cmds)
(require 'nav)
(require 'load-theme-buffer-local)
(require 'rainbow-delimiters)
(require 'smart-tab)
(require 'tern)

;(autoload 'tern-mode "tern.el" nil t)

; enable/disable certain features
(scroll-bar-mode -1)              ; disable scroll bar
(tool-bar-mode -1)                ; disable tool bar 
(setq initial-scratch-buffer nil)
(turn-on-follow-mouse)            ; turn on focus follows mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; scroll one line at a time
(savehist-mode 1) ; persist minibuffer history across sessions
(setq savehist-file "~/.emacs.d/savehist") ; set file to save history
(setq ring-bell-function 'ignore) ; stop bell
(global-smart-tab-mode 1) ; enable smart tabbing
(global-flycheck-mode)

; configure SLIME
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf slime-banner))
; set-up SLIME autocomplete
(defun my-slime-mode-hook ()
  (set-up-slime-ac)
  (rainbow-delimiters-mode))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

; configure common lisp mode
(defun my-common-lisp-mode-hook ()
  (linum-mode)
  (rainbow-delimiters-mode))

; configure scheme repl
(setq scheme-program-name "mit-scheme")
(defun my-inferior-scheme-mode-hook ()
  (require 'quack)
  (setq quack-fontify-style 'emacs)
  (rainbow-delimiters-mode)
)

; configure scheme files
(defun my-scheme-mode-hook ()
  (linum-mode)
  (rainbow-delimiters-mode)
)

; configure javascript files
(defun my-js2-mode-hook ()
  (linum-mode)
  (tern-mode t))

(add-hook 'slime-mode-hook 'my-slime-mode-hook)
(add-hook 'slime-repl-mode-hook 'my-slime-mode-hook)
(add-hook 'lisp-mode-hook 'my-common-lisp-mode-hook)
(add-hook 'inferior-scheme-mode-hook 'my-inferior-scheme-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(require 'smartparens-config)
(smartparens-global-mode t)

; set indentation options
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; enable autocompletion popup
(require 'auto-complete-config)
(ac-config-default)

; set directory to save backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; enable and configure vi emulation
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "ii" 'evil-normal-state)
(key-chord-mode 1)

; set default font and theme
(set-default-font "InconsolataLGC-12")
(load-theme 'solarized-dark t)

; set default layout
;(setq w (selected-window))
;(setq w2 (split-window w -16))
;(select-window w2)
;(ansi-term "zsh")
;(load-theme-buffer-local 'solarized-light t)
;(select-window w)
(nav)

; toggle fullscreen on OSX
(cond
 ((string-match "darwin" system-configuration)
  (toggle-frame-fullscreen)
 )
)
