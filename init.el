;;; init.el --- Emacs config -*- lexical-binding: t; -*-
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
;;    Retrieve from old repository;
;;    (https://github.com/nukosuke/dotfiles/.emacs.d).

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el
;; - package management with version lock system
;; - https://github.com/raxod502/straight.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with use-package macro
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(define-key global-map (kbd "C-c p") 'straight-dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-dolist (pkg
            '((generic    "generic")
              (interface  "interface")
              (workspace  "workspace")
              (completion "completion")
              (vcs        "vcs")

              ;; Dispatchers
              (straight-dispatch "dispatcher/straight-dispatch")
              (zoom-dispatch     "dispatcher/zoom-dispatch")
              (counsel-dispatch  "dispatcher/counsel-dispatch")

              ;; Text
              (markdown "language/markdown")
              (yaml     "language/yaml")

              ;; Config
              (gitignore "language/gitignore")
              (terraform "language/terraform")

              ;; Programming language
              (typescript "language/typescript")
              (web        "language/web")
              (graphql    "language/graphql")
              (golang     "language/golang")))
  (let ((name (car pkg))
        (path (nth 1 pkg)))
    (require name (concat user-emacs-directory "site-lisp/" path))))

;; (if (executable-find "mu")
;;    (require 'mailer
;;             (concat user-emacs-directory "site-lisp/mailer")))

(provide 'init)

;;; init.el ends here
