;;; language.el --- lang-mode configurations -*- lexical-binding: t -*-
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
;;  * 2019/04/22:
;;    Create language.el

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Erlang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erlang
  :mode "\\.erl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :mode "\\.ex" "\\.exs")

(use-package alchemist
  :after elixir-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  :mode "\\.rs\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for general markup lang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.erb\\'"   . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset    2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for TypeScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  ;; defer load until open any .ts file.
  :mode
  "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tide:
;;   TypeScript Interactive Development Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tide
  :after
  (typescript-mode company flycheck)
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save     . tide-format-before-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Elm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elm-mode
  :mode "\\.elm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for ProtocolBuffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package protobuf-mode
  :mode "\\.proto'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Vue.js template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vue-mode
  :mode
  ("\\.vue" . vue-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :mode
  ("\\.ya?ml\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Dockerfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Terraform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :mode
  ("\\.tf\\'" . terraform-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for .gitignore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gitignore-mode
  :mode
  ("\\'\\.gitignore\\'" . gitignore-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :mode
  ;; Use GitHub Flavored Markdown instead of Common Markdown
  ("\\.md\\'"       . gfm-mode)
  ("\\.markdown\\'" . gfm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extension for Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto insert TOC section
;; https://github.com/snosov1/toc-org
(use-package toc-org
  :requires
  toc-org
  :hook
  (org-mode toc-org-mode))

(provide 'language)

;;; language.el ends here
