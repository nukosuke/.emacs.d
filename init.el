;;; init.el --- Emacs config -*- lexical-binding: t; -*-
;;
;; Copyright (c) nukosuke
;;
;; Author: nukosuke <nukosuke@lavabit.com>
;;
;;; Commentary:
;;
;;  2019/02/26:
;;    Retrieve from old repository;
;;    (https://github.com/nukosuke/dotfiles/.emacs.d).

;;; Code:

;; Use straight.el for package management.

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

(provide 'init)

;;; init.el ends here

