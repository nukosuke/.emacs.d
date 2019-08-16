;;; shell.el --- Shell settings -*- lexical-binding: t; -*-
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
;;  * 2019/08/16:
;;    Create shell.el
;;    - Move term-mode config from interface.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package term
  :straight nil
  :bind
  ("C-T" . ansi-term)   ;; new ansi-term
  (:map term-raw-map
        ("C-T" . nil)   ;; recover for ansi-term
        ("M-x" . nil)   ;; recover for cousel-M-x
        ("M-o" . nil))) ;; recover for ace-window

(provide 'shell)

;;; shell.el ends here
