;;; calendar.el --- Calendar settings -*- lexical-binding: t; -*-
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
;;  * 2019/03/26:
;;    Create calendar.el
;;    - calfw, calfw-org settings

;;; Code:

(use-package calfw)

(use-package calfw-org
  :after calfw)

(provide 'calendar)

;;; calendar.el ends here
