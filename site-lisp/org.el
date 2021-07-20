;;; org.el --- org-mode configurations -*- lexical-binding: t -*-
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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extension for Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto insert TOC section
;; https://github.com/snosov1/toc-org
(use-package toc-org
  :requires
  toc-org
  :hook
  (org-mode toc-org-mode))

;; Static site generator
;; https://github.com/bastibe/org-static-blog
(use-package org-static-blog
  :commands
  org-static-blog-create-new-post
  :hook
  (org-mode . org-static-blog-mode)
  :custom
  (org-static-blog-publish-title   "nukosuke's blog")
  (org-static-blog-publish-url     "https://blog.nukosuke.com/")
  (org-static-blog-publish-directory "~/blog/public/posts")
  (org-static-blog-enable-tags     t)
  (org-export-with-toc             nil)
  (org-export-with-section-numbers nil))

(provide 'org)
