;;; golang.el --- Go mode settings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  This file is NOT part of GNU Emacs.
;;
;;  * 2019/02/27:
;;    Create golang.el
;;    - Configure go-mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :mode
  ("\\.go\\'" . go-mode)

  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")))

;;;###autoload
(defun install-language-server/golang ()
  "Install language server, bingo for golang.  GO111MODULE environment variable should be set to 'on'."
  (interactive)
  (if (executable-find "go")
      (message (shell-command-to-string "go get -u github.com/saibing/bingo"))
    (message "Couldn't find go. Install golang first.")))

(provide 'golang)

;;; golang.el ends here
