;;; basic-mode.el --- Syntax highlighting for BASIC

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: lang syntax
;; URL: http://gitlab.com/elzair/basic-mode.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Syntax highlighting for BASIC code

;;; Code:

(require 'generic-x)

(define-generic-mode
    'basic-mode
  '("REM")
  '("DEF" "DIM" "END" "STOP" "FOR" "TO" "STEP" "NEXT" "GOSUB" "RETURN"
    "GOTO" "IF" "THEN" "LET" "PRINT" "DATA" "READ" "INPUT")
  '(("=" . 'font-lock-operator)
    ("+" . 'font-lock-operator)
    ("-" . 'font-lock-operator)
    ("*" . 'font-lock-operator)
    ("/" . 'font-lock-operator)
    ("<" . 'font-lock-operator)
    ("=" . 'font-lock-operator)
    (">" . 'font-lock-operator)
    (":" . 'font-lock-builtin)
    (";" . 'font-lock-builtin)
    ("^[:"))
  '("\\.bas$")
  nil
  "Major mode for BASIC programming language.")

(provide 'basic-mode)
;;; basic-mode.el ends here
