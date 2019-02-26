;;; 150_lang-modes.el -- 言語モードの設定 -*- lexical-binding: t -*-
;;; Commentary:
;;
;; TODO: LSPの導入
;;
;;; Code:

;; Shell
(use-package fish-mode)

;; Web
(use-package web-mode :mode "\\.html\\'" "\\.css\\'" "\\.php\\'" "\\.jsx\\'")
(use-package js2-mode :mode "\\.js\\'")
(use-package json-mode)
(use-package slim-mode)
(use-package markdown-mode)

;; Conf
(use-package yaml-mode)
(use-package cmake-mode)
(use-package dockerfile-mode)
(use-package terraform-mode)
(use-package gitignore-mode)

;; TypeScript
(use-package typescript-mode)
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :diminish)

;; Ruby
(use-package ruby-mode
  :init
  (custom-set-variables '(ruby-insert-encoding-magic-comment nil)))

;; Elixit
(use-package elixir-mode)

;; Go
(use-package go-mode)

;; Rust
(use-package rust-mode)

;; Lua
(use-package lua-mode)

;; etc
(use-package protobuf-mode)

(provide '150_lang-modes)

;;; 150_lang-modes.el ends here
