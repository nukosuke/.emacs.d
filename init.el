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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Emacs Lisp
;; FIXME: more dry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General settings
(require 'generic (concat user-emacs-directory "site-lisp/generic"))

;; User interface settings
(require 'interface (concat user-emacs-directory "site-lisp/interface"))

;; Workspace settings
(require 'workspace (concat user-emacs-directory "site-lisp/workspace"))

;; Completion settings
(require 'completion (concat user-emacs-directory "site-lisp/completion"))

;; Version Control System settings
(require 'vcs (concat user-emacs-directory "site-lisp/vcs"))

;; Dispatchers (transient commands)
(require 'straight-dispatch
         (concat user-emacs-directory "site-lisp/dispatcher/straight-dispatch"))
(require 'zoom-dispatch
         (concat user-emacs-directory "site-lisp/dispatcher/zoom-dispatch"))

;; Structured text format
(require 'markdown
         (concat user-emacs-directory "site-lisp/language/markdown"))
(require 'yaml
         (concat user-emacs-directory "site-lisp/language/yaml"))

;; Config format
(require 'gitignore
         (concat user-emacs-directory "site-lisp/language/gitignore"))
(require 'terraform
         (concat user-emacs-directory "site-lisp/language/terraform"))

;; Programming languages
(require 'typescript
         (concat user-emacs-directory "site-lisp/language/typescript"))
(require 'golang
         (concat user-emacs-directory "site-lisp/language/golang"))
(require 'web
         (concat user-emacs-directory "site-lisp/language/web"))
(require 'graphql
         (concat user-emacs-directory "site-lisp/language/graphql"))

(provide 'init)

;;; init.el ends here
