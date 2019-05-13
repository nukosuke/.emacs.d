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

(use-package ddskk
  :commands
  skk-mode

  :custom
  (skk-sticky-key  ";")
  (skk-show-inline 'vertical)

  ;; Usse AZIK input method
  (skk-use-azik t)
  (skk-azik-keyboard-type 'us101)

  ;; Use doom-dracula colors
  ;; https://github.com/hlissner/emacs-doom-themes/blob/master/themes/doom-dracula-theme.el#L38-L67
  (skk-cursor-hiragana-color "#61bfff")
  (skk-cursor-katakana-color "#ff79c6")

  :init
  ;; SKK faces should be set before load for customizing
  (defface skk-emacs-hiragana-face nil nil)
  (set-face-attribute 'skk-emacs-hiragana-face nil
                      :background "#bd93f9"
                      :foreground "black"
                      :bold t)
  (defface skk-emacs-katakana-face nil nil)
  (set-face-attribute 'skk-emacs-katakana-face nil
                      :background "#bd93f9"
                      :foreground "black"
                      :bold t)

  :bind
  ("C-c j" . skk-mode))

(provide 'ime)

;;; ime.el ends here