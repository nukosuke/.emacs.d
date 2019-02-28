;;; markdown.el --- Markdown mode settings -*- lexical-binding: t; -*-
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
;;  * 2019/02/27:
;;    Create markdown.el
;;    - Configure markdown-mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :mode
  ;; Use GitHub Flavored Markdown instead of Common Markdown
  ("\\.md\\'"       . gfm-mode)
  ("\\.markdown\\'" . gfm-mode))

(provide 'markdown)

;;; markdown.el ends here
