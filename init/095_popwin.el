;;; 095_popwin.el -- ポップウィンドウの設定 -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; 別バッファを入れ替えず一時的に新しいバッファをポップアップする
;;
;; TODO:
;; file番号を最初の方に移す
;;
;;; Code:

(use-package shackle
  :config
  (progn
    (setq helm-display-function 'pop-to-buffer)
    (setq shackle-rules
          '(("*Wargings*" :same t)
            ("\*Buffer List" :regexp t :select t :align t :size 0.4)
            ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
            ("\\*eshell\\*" :regexp t :popup t :select t :align t :size 0.2)
            ("*git-gutter:diff*" :popup t :select nil :align t :size 0.4)))
    (shackle-mode)
    (winner-mode)
    (global-set-key (kbd "C-z") 'winner-undo)))

(provide '095_popwin)

;;; 095_popwin.el ends here
