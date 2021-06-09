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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-dolist (pkg
            '((generic    "generic")
              (ime        "ime")
              (interface  "interface")
              (workspace  "workspace")
              (completion "completion")
              (vcs        "vcs")
              (language   "language")
              (dispatch   "dispatch")))
  (let ((name (car pkg))
        (path (nth 1 pkg)))
    (require name (concat user-emacs-directory "site-lisp/" path))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(current-language-environment "UTF-8")
 '(frame-title-format (format "GNU Emacs %s" emacs-version) t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(org-agenda-files nil)
 '(org-return-follows-link t)
 '(org-use-speed-commands t)
 '(ring-bell-function 'ignore)
 '(scroll-step 1)
 '(select-enable-clipboard t)
 '(tab-width 2)
 '(viper-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 3.0 :foreground "#f1fa8c")))))
