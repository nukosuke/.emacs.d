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
  :straight (:type built-in)

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
  (denote-date-prompt-use-org-read-date t)

  :bind
  ("C-c n" . denote-dispatch)

  :config
  (transient-define-prefix denote-dispatch ()
    "Invoke a denote.el command from a list of available commands."
    ["Denote"
     ["Create"
      ("n" "New note" denote)
      ("N" "New note with file type" denote-type)
      ("d" "New note for date and time" denote-date)
      ("s" "New note into subdirectory" denote-subdirectory)]
     ["Rename"
      ("r" "Rename file" denote-rename-file)
      ("R" "Rename file using front matter" denote-rename-file-using-front-matter)]]
    ["Link"
     ("i" "Insert link to target note" denote-link)
     ("I" "Insert links to all notes matching regex" denote-link-add-links)
     ("b" "Show backlinks" denote-link-backlinks)
     ("f f" "Find linked file" denote-link-find-file)
     ("f b" "Find backlinked file" denote-link-find-backlink)]))

(provide 'note)

;;; note.el ends here
