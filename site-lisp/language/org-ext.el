;;; org-ext.el --- Org extensions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Auto insert TOC section
;; https://github.com/snosov1/toc-org
(use-package toc-org
  :requires
  toc-org

  :hook
  (org-mode toc-org-mode))

(provide 'org-ext)

;;; org-ext.el ends here
