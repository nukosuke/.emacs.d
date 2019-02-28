;;; interface.el --- User interface settings -*- lexical-binding: t; -*-
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
;;  * 2019/02/26:
;;    Create interface.el
;;    - Configure UI

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disabled modes
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

;; Enabled modes
(winner-mode         1)
(global-hl-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish: This package implements hiding or
;;           abbreviation of the mode line displays
;;           (lighters) of minor-modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; diminish is no longer dependencies of use-package
;; So, install here.
(use-package diminish)

;; volatile-highlights:
;;   Minor mode for visual feedback on some operations
;; TODO:
;; - Fix vhl/default-face
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :init
  (require 'seq)

  :custom
  (dashboard-banner-logo-title (concat "GNU Emacs " emacs-version))
  (dashboard-startup-banner    'logo)

  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Display line number
;;
(cl-dolist (hook (list
                  'prog-mode-hook
                  'text-mode-hook
                  'conf-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode))))

;;
;; Display VCS diff marker
;;
(use-package git-gutter-fringe
  :config
  ;; Custom fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added    '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted  'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")
  (global-git-gutter-mode t))

;;
;; Flycheck
;; FIXME: move to appropriate file.
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)

  :custom
  ;; Because left-fringe is used by git-gutter-fringe
  (flycheck-indication-mode 'right-fringe)

  :config
  ;; Custom fringe bitmap
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X...."))

(provide 'interface)

;;; interface.el ends here
