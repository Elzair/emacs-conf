;;; presentation.el --- code for toggling full-screen mode
;;; Commentary:
;;; This is my code for entering full-screen mode. It assumes the only two
;;; windows in the current frame are neotree and another buffer.
;;; Code:

(defvar presentation-saved-mode-line-format nil
  "The mode-line format for neotree.")
(defvar presentation-saved-window-linum-mode 1
  "Toggle for whether or not saved-window-linum-mode is active.")
(defvar presentation-show-menu-bar 1
  "Toggle for whether or not menu bar is shown.")

(defun toggle-presentation ()
  "This function will toggle presentation mode."
  (interactive)
  (progn
    (toggle-frame-fullscreen)
    (if (eq presentation-saved-mode-line-format nil)
        (progn
          (neotree-hide)
          (setq presentation-saved-mode-line-format (list mode-line-format))
          (setq mode-line-format nil))
        (setq mode-line-format presentation-saved-mode-line-format)
        (setq presentation-saved-mode-line-format nil)
        (neotree-show)
        (other-window 1))
    (setq presentation-saved-window-linum-mode
          (- presentation-saved-window-linum-mode))
    (linum-mode presentation-saved-window-linum-mode)
    (setq presentation-show-menu-bar
          (- presentation-show-menu-bar))
    (menu-bar-mode presentation-show-menu-bar)))

(provide 'presentation)
;;; presentation.el ends here
