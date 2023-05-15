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
;;
;;  * 2019/08/22:
;;    Fix void function error caused by
;;    https://github.com/ema2159/centaur-tabs/commit/de3738c14b8e73e135c16e26ca405f18459fbb20
;;    - Remove centaur-tabs-inherit-tabbar-faces
;;
;;  * 2019/09/04:
;;    - Add term-mode config
;;    - Add docker.el
;;
;;  * 2019/12/18:
;;    Customize eshell prompt
;;
;;  * 2022/11/07:
;;    Use built-in modus themes
;;
;;  * 2022/11/09:
;;    Move terminal settings to term.el
;;
;;  * 2023/05/15
;;    Use nerd-icons instead of all-the-icons

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initial ALIST
(when (memq window-system '(mac ns))
  (setq default-frame-alist
        (append '((ns-transparent-titlebar . t)
                  (ns-appearance           . dark))))) ;; supported in 26.1 (light or dark)))

;; Deafult local variable
(setq-default indicate-empty-lines       'left
              indicate-buffer-boundaries 'left)

;; Enabled modes
(winner-mode         1)
(global-hl-line-mode t)
(pixel-scroll-mode   1)
;; for Emacs >= 29
;; (pixel-scroll-precision-mode 1)

(use-package paren
  :straight nil

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
  (beacon-color "#2fafff") ; modus-theme blue

  :config
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :straight nil

  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  :config
  (load-theme 'modus-vivendi))

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
  :custom
  (dashboard-banner-logo-title (concat "GNU Emacs " emacs-version))
  (dashboard-startup-banner    'logo)
  (dashboard-icon-type         'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons    t)
  (dashboard-projects-backend  'project-el)
  (dashboard-items             '((recents  . 5)
                                 (projects . 5)
                                 (agenda   . 5)))

  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package centaur-tabs
  :hook
  ((calendar-mode
    dashboard-mode
    denote-backlinks-mode
    eshell-mode
    git-commit-setup ;; magit COMMIT_MSG buffer
    helpful-mode
    ibuffer-mode
    imenu-list-major-mode
    org-agenda-mode
    term-mode
    vterm-mode
    mastodon-mode
    ement-mode) . centaur-tabs-local-mode)

  :custom
  (centaur-tabs-style               "bar")
  (centaur-tabs-set-bar             'left)
  (centaur-tabs-set-icons           t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker     "*")

  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

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
;; Display fill column indicator
;;
(add-hook 'text-mode-hook (lambda ()
                            (display-fill-column-indicator-mode)))

;;
;; Display indent
;;

(use-package highlight-indent-guides
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-method       'bitmap))

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
  (set-face-attribute 'git-gutter-fr:added nil
                      :foreground (modus-themes-get-color-value 'green)
                      :background (modus-themes-get-color-value 'fringe))
  (set-face-attribute 'git-gutter-fr:modified nil
                      :foreground (modus-themes-get-color-value 'yellow)
                      :background (modus-themes-get-color-value 'fringe))
  (set-face-attribute 'git-gutter-fr:deleted nil
                      :foreground (modus-themes-get-color-value 'red)
                      :background (modus-themes-get-color-value 'fringe))
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
    "...X....")
  (set-face-attribute 'flycheck-fringe-error nil
                      :foreground (modus-themes-get-color-value 'red)
                      :background (modus-themes-get-color-value 'fringe))
  (set-face-attribute 'flycheck-fringe-warning nil
                      :foreground (modus-themes-get-color-value 'yellow)
                      :background (modus-themes-get-color-value 'fringe))
  (set-face-attribute 'flycheck-fringe-info nil
                      :foreground (modus-themes-get-color-value 'blue)
                      :background (modus-themes-get-color-value 'fringe)))

;;
;; Display flycheck message to posframe
;;

(use-package flycheck-posframe
  :after flycheck

  :hook
  (flycheck-mode . flycheck-posframe-mode)

  :config
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning))

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
  (imenu-list-mode-line-format       nil)

  ;; Close Ilist frame after jump
  :hook
  (imenu-list-after-jump . imenu-list-smart-toggle)

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
;; Emacs integration for Docker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package docker
  :bind
  ("C-c d" . docker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyanyanyanyanyanyanyanyanyan!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nyan-mode
  :hook
  ((prog-mode text-mode conf-mode) . nyan-mode))

(provide 'interface)

;;; interface.el ends here
