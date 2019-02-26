;;; generic.el --- General settings -*- lexical-binding: t; -*-
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
;;    Create generic.el
;;    - Configure custom-set-variables

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; Variable                    Value
 '(current-language-environment "UTF-8")
 ;; FIXME: emacs-version includes build number from Emacs 27.0.50
 ;;   Want just only "GNU Emacs <major>.<minor>.<patch>"
 '(frame-title-format           (emacs-version))
 '(indent-tabs-mode             nil)
 '(make-backup-files            nil)
 '(show-paren-mode              t)
 '(tab-width                    2)
 '(x-select-enable-clipboard    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO:

(provide 'generic)

;;; generic.el ends here
