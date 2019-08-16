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
;;
;;  * 2019/07/29:
;;    Add tab UI
;;    - Configure centaur-tabs

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

(use-package paren
  :straight nil

  :hook
  (after-init . show-paren-mode)

  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volatile-highlights:
;;   Minor mode for visual feedback on some operations
;; beacon:
;;   A light that follows your cursor around
;;   so you don't lose it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

(use-package beacon
  :custom
  (beacon-color "#f1fa8c") ;; yellow of dracula

  :config
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold   t)

  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)

  :config
  (line-number-mode 0)

  :init
  (doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :init
  (require 'seq)

  :custom
  (dashboard-banner-logo-title (concat "GNU Emacs " emacs-version))
  (dashboard-startup-banner    'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons    t)
  (dashboard-items             '((recents  . 5)
                                 (projects . 5)
                                 (agenda   . 5)))

  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package centaur-tabs
  :straight (centaur-tabs :host github
                          :repo "ema2159/centaur-tabs")

  :demand ;; Load immediately

  :custom
  (centaur-tabs-style               "bar")
  (centaur-tabs-set-bar             'left)
  (centaur-tabs-set-icons           t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker     "*")

  :config
  (centaur-tabs-mode t)
  (centaur-tabs-inherit-tabbar-faces)

  :bind
  ("C-c ,"     . centaur-tabs-counsel-switch-group)
  ("M-<left>"  . centaur-tabs-backward-tab)
  ("M-<right>" . centaur-tabs-forward-tab))

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
;; Display indent
;;
(use-package highlight-indent-guides
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)

  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive   t)
  (highlight-indent-guides-method 'character))

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

;;
;; Display flycheck message to posframe
;;
;; NOTE: flycheck-posframe is buggy on macOS when fullscreen toggle.
;;       It creates another blackout workspace and draw frame to it.
;; (use-package flycheck-posframe
;;  :after
;;  flycheck
;;
;;  :hook
;;  (flycheck-mode . flycheck-posframe-mode))

;;
;; Display flycheck message to tip
;;
(use-package flycheck-pos-tip
  :after
  flycheck

  :hook
  (flycheck-mode . flycheck-pos-tip-mode))

;;
;; Hide unnecessary mode line
;;
(use-package hide-mode-line
  :hook
  (neotree-mode . hide-mode-line-mode))

;;
;; Switching window with jump
;;
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "#f1fa8c"))))

  :bind
  ("M-o" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :bind
  ("M-a" . avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize            nil)

  ;; Don't use hide-mode-line-mode
  ;; It hide major mode mode-line after toggle
  (imenu-list-mode-line-format nil)

  :bind
  ("C-'" . imenu-list-smart-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful: A better Emacs *help* buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helpful
  :bind
  ("C-c h v" . helpful-variable)
  ("C-c h k" . helpful-key)
  ("C-c h c" . helpful-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iedit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package iedit
  :bind
  ("C-;" . iedit-mode)
  (:map iedit-mode-keymap
        ("C-h" . delete-backward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minimap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-update-delay    0.2)
  (minimap-minimum-width   20)
  :bind
  ("C-c m" . minimap-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyanyanyanyanyanyanyanyanyan!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nyan-mode
  :hook
  ((prog-mode text-mode conf-mode) . nyan-mode))

(provide 'interface)

;;; interface.el ends here
