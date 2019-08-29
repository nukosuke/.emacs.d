;;; vcs.el --- General settings -*- lexical-binding: t; -*-
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
;;    Create vcs.el
;;    - Configure integration of Version Control System

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit: A Git Porcelain inside Emacs
;;        https://magit.vc/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind
  ;; key is NOT mapped after magit load.
  ;; So, manualy mapping is required.
  ("C-c C-c" . magit-dispatch)
  ("C-x g"   . magit-status)
  (:map org-mode-map
        ;; bind override by magit and apply other bind
        ("C-c C-c" . magit-dispatch)
        ("C-c c"   . org-ctrl-c-ctrl-c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forge: Work with Git forges from the comfort of Magit
;;        https://github.com/magit/forge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package forge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; github-review: GitHub code reviews with Emacs
;;                https://github.com/charignon/github-review
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package github-review)

(provide 'vcs)

;;; vcs.el ends here
