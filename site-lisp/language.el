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

(use-package elixir-mode
  :mode "\\.ex" "\\.exs")

(use-package elm-mode
  :mode "\\.elm")

(provide 'language)

;;; language.el ends here
