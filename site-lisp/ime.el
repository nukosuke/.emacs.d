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
;;
;;  * 2021/02/16:
;;    Add migemo(cmigemo) config and integrate with ivy-swiper
;;    - https://github.com/emacs-jp/migemo

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Daredevil SKK: Simple Kana to Kanji conversion program,
;;                an input method of Japanese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ddskk
  :commands
  skk-mode

  :custom
  (skk-jisyo-code 'utf-8)
  (skk-show-inline 'vertical)

  ;; Usse AZIK input method
  (skk-use-azik t)
  (skk-azik-keyboard-type 'us101)

  ;; Use modus-vivendi colors
  (skk-cursor-hiragana-color "#2fafff") ;; (modus-themes-get-color-value 'blue)
  (skk-cursor-katakana-color "#feacd0") ;; (modus-themes-get-color-value 'magenta)

  :init
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migemo: Japanese increment search
;;         with 'Romanization of Japanese'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package migemo
  :if (executable-find "cmigemo")
  :after ivy

  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-coding-system 'utf-8-unix)

  ;; Use cmigemo with Swiper
  (ivy-re-builders-alist '((t . ivy--regex-plus)
                           (swiper . migemo-ivy-re-builder)))

  :init
  (defun migemo-ivy-re-builder (str)
    (let* ((sep " \\|\\^\\|\\.\\|\\*")
           (splitted (--map (s-join "" it)
                            (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                            (s-split "" str t)))))
      (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                              ((s-matches? sep it) it)
                              (t (migemo-get-pattern it)))
                        splitted))))

  :config
  (migemo-init))

(provide 'ime)

;;; ime.el ends here
