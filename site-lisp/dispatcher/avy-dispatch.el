;;; avy-dispatch.el --- Avy commands -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  These files are NOT part of GNU Emacs.
;;
;;  * 2019/03/06:
;;    Create counsel-dispatch.el
;;    - Define transient commands for counsel

;;; Code:

(require 'transient)

(transient-define-prefix avy-dispatch ()
  "Invoke a Avy command from a list of available commands."
  ["Avy commands\n"
   ["Line"
    ("y" "Yank" avy-copy-line)
    ("m" "Move" avy-move-line)
    ("k" "Kill" avy-kill-whole-line)]

   ["Region"
    ("Y" "Yank" avy-copy-region)
    ("M" "Move" avy-move-region)
    ("K" "Kill" avy-kill-region)]

   ["Goto"
    ("c" "Timed char" avy-goto-char-timer)
    ("C" "Char"       avy-goto-char)
    ("w" "Word"       avy-goto-word-1)
    ("W" "Any word"   avy-goto-word-0)
    ("l" "Line"       avy-goto-line)]])

(global-set-key (kbd "C-c a") 'avy-dispatch)

(provide 'avy-dispatch)

;;; avy-dispatch.el ends here
