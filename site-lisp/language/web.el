;;; web.el --- Web mode settings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  This file is NOT part of GNU Emacs.
;;
;;  * 2019/03/05:
;;    Create web.el
;;    - Configure web-mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.erb\\'"   . web-mode)

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset    2))

(provide 'web)

;;; web.el ends here
