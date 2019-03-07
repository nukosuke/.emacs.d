;;; mailer.el --- RMAIL config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rmail
  :straight nil ;; rmail is built-in Emacs

  :custom
  (rmail-remote-password-required t)
  (rmail-primary-inbox-list '("imaps://yosuke.tamura%40mixi.co.jp@imap.gmail.com:993"))

  :config
  (setenv "MAILHOST" "lavabit.com"))

(provide 'mailer)

;;; mailer.el ends here
