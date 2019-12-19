;;; ime.el --- input method settings -*- lexical-binding: t; -*-
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

;;; Code:

(use-package skk
  :straight nil

  :load-path
  "vendor/sskk"

  :commands
  skk-mode

  :custom
  (skk-show-inline 'vertical)

  ;; Usse AZIK input method
  (skk-use-azik t)
  (skk-azik-keyboard-type 'us101)

  ;; Use doom-dracula colors
  ;; https://github.com/hlissner/emacs-doom-themes/blob/master/themes/doom-dracula-theme.el#L38-L67
  (skk-cursor-hiragana-color "#61bfff")
  (skk-cursor-katakana-color "#ff79c6")

  :init
  (require 'skk-autoloads)

  :config
  ;; SKK faces should be set before load for customizing
  (defface skk-emacs-hiragana-face
    '((t (:inherit (highlight bold)))) nil)

  (defface skk-emacs-katakana-face
    '((t (:inherit (highlight bold)))) nil)

  (defface skk-emacs-jisx0208-latin-face
    '((t (:inherit (highlight bold)))) nil)

  (defface skk-emacs-jisx0201-face
    '((t (:inherit (highlight bold)))) nil)

  (defface skk-emacs-abbrev-face
    '((t (:inherit (highlight bold)))) nil)

  :bind
  ("C-c j" . skk-mode))

(provide 'ime)

;;; ime.el ends here
