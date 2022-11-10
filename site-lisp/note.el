;;; note.el --- note taking configurations -*- lexical-binding: t -*-
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
;;  * 2021/06/11:
;;    Create org.el
;;
;;  * 2022/11/08:
;;    Setup denote

;;; Code:

(require 'transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extension for Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :straight nil

  :custom
  (org-latex-classes '(("ltjsarticle" "\\documentclass[a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{graphicx}
\\usepackage{hyperref}")))
  ;; (org-export-latex-default-packages-alist (("" "luatexja" nil)
  ;;                                            ("" "graphicx" nil)
  ;;                                            ("" "hyperref" nil)))
  (org-latex-pdf-process '("latexmk -lualatex %f"))
  (org-export-in-background nil))

;; Auto insert TOC section
;; https://github.com/snosov1/toc-org
(use-package toc-org
  :requires
  toc-org

  :hook
  (org-mode . toc-org-mode))

(transient-define-prefix org-dispatch ()
  "Dispatch org menu."
  ["org commands\n"
   ["org"
    ("e" "Export" org-export-dispatch)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple notes for Emacs with an efficient file-naming scheme
;; https://protesilaos.com/emacs/denote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package denote
  :custom
  (denote-directory (concat user-emacs-directory "notes/"))
  (denote-backlinks-show-context t)

  :bind
  ;; TODO: create transient prefix C-n
  ("C-c n n" . denote)
  ("C-c n N" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n t" . denote-template)
  ("C-c n i" . denote-link)
  ("C-c n I" . denote-link-add-links)
  ("C-c n b" . denote-link-backlinks)
  ("C-c n f f" . denote-link-find-file)
  ("C-c n f b" . denote-link-find-backlink)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter))

(provide 'note)

;;; note.el ends here
