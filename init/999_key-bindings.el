;;; 999_key-bindings.el -- キーバインドの設定 -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; パッケージを使用しないデフォルトのバインドを変える場合の設定
;; 基本的に各パッケージのキーバインドはそれぞれの設定ファイルで行う
;;
;;; Code:

;; sequential-command
;; C-a 行頭 -> ファイルの先頭
;; C-e 行末 -> ファイルの末尾
(use-package sequential-command
  :init
  (require 'sequential-command-config)
  :bind
  ("C-a" . seq-home)
  ("C-e" . seq-end))

;;
;; その他グローバルキーバインド
;;
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-m") 'set-mark-command)

(provide '999_key-bindings)

;;; 999_key-bindings.el ends here
