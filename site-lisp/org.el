;;; org.el --- Org mode settings -*- lexical-binding: t; -*-
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
;;    Create org.el
;;    - Configure org-mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode is for keeping notes, maintaining TODO lists,
;;          planning projects, and authoring documents
;;          with a fast and effective plain-text system
;;
;;                 https://orgmode.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :straight nil ;; Use Emacs built-in version
  
  :custom
  (org-return-to-follows-link t)
  (org-use-speed-commands     t))

(provide 'org)

;;; org.el ends here
