;;; mailer.el --- RMAIL config -*- lexical-binding: t; -*-
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
;;  * 2019/03/07:
;;    Create mailer.el
;;    - Configure IMAP/SMTP for Lavabit

;;; Code:

(use-package rmail
  :straight nil ;; rmail is built-in Emacs

  :custom
  ;; Receiving: IMAP/SSL
  (rmail-remote-password-required t)
  (rmail-primary-inbox-list '("imaps://nukosuke@lavabit.com:993"))

  ;; Sending: SMTP/SSL
  (user-mail-address     "nukosuke@lavabit.com")
  (smtpmail-smtp-server  "lavabit.com")
  (send-mail-function    'smtpmail-send-it)
  (smtpmail-stream-type  'ssl)
  (smtpmail-smtp-service 465))

(provide 'mailer)

;;; mailer.el ends here
