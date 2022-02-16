;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  These files are NOT part of GNU Emacs.
;;
;;  * 2022/02/16:
;;    Move deactivation for menu-bar, tool-bar and
;;    scroll-bar from site-lisp/interface.el to
;;    early-init.el;

;;; Code:

;; Set default values before GUI initialization
(push '(menu-bar-lines   . 0) default-frame-alist)
(push '(tool-bar-lines   . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; for straight.el
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
