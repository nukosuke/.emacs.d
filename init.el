;;; init.el --- yet another init.el
;; Copyright (c) nukosuke
;; Author: nukosuke <nukosuke@cpan.org>
;;; Commentary:
;;; Code:

;; パッケージマネージャにstraight.elを使う
;; https://github.com/raxod502/straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをstraightにフォールバック
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package el-patch)
(use-package init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/init")

(provide 'init)
;;; init.el ends here
