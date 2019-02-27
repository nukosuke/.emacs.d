;;; zoom-dispatch.el --- Zoom transient command -*- lexical-binding: t; -*-
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
;;  * 2019/02/27:
;;    Create zoom-dispatch.el
;;    - Define transient command to zoom in/out window.

;;; Code:

(require 'transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x zoom-dispatch
;;
;; g : Zoom IN
;; l : Zoom OUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-transient-command zoom-dispatch ()
  "Invoke a zoom command from a list of available commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Zoom commands"
   ("g" "Zoom IN" text-scale-increase)
   ("l" "Zoom OUT" text-scale-decrease)])

(provide 'zoom-dispatch)

;;; zoom-dispatch.el ends here
