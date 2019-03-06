;;; mailer.el --- Gnus config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gnus
  :straight nil

  :custom
  (user-full-name "Yosuke Tamura")
  (user-mail-address "yosuke.tamura@mixi.co.jp")
  (gnus-select-method '(nnimap "gmail"
                               (nnimap-address "imap.gmail.com")
                               (nnimap-server-port 993)
                               (nnimap-stream ssl))))

(provide 'mailer)

;;; mailer.el ends here
