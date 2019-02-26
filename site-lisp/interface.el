;;; interface.el --- User interface settings -*- lexical-binding: t; -*-
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
;;  * 2019/02/26:
;;    Create interface.el
;;    - Configure UI

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disabled modes
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line number
(cl-dolist (hook (list
                  'prog-mode-hook
                  'text-mode-hook
                  'conf-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode))))

(provide 'interface)

;;; interface.el ends here
