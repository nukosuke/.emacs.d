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

;; Disabled modes
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode   0)
      (menu-bar-mode   0)))

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

  :hook
  ((dashboard-mode
    term-mode
    calendar-mode
    org-agenda-mode
    helpful-mode) . centaur-tabs-local-mode)

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
(when (version<= "27.0.50" emacs-version)
  (add-hook 'text-mode-hook (lambda ()
                              (display-fill-column-indicator-mode))))

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
(use-package flycheck-posframe
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode))

;; Or this is alternative
;;
;; (use-package flycheck-pos-tip
;;  :after
;;  flycheck
;;
;;  :hook
;;  (flycheck-mode . flycheck-pos-tip-mode)
;;
;;  :custom
;;  (flycheck-pos-tip-timeout 30))

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
;; ansi-term keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package term
  :straight nil
  :bind
  ("C-c t" . ansi-term)   ;; new ansi-term
  (:map term-raw-map
        ("C-c t" . nil)   ;; recover for ansi-term
        ("M-x"   . nil)   ;; recover for cousel-M-x
        ("M-o"   . nil))) ;; recover for ace-window

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom eshell prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eshell
  :straight nil
  :hook eshell
  :custom
  ;; Eshell prompt header
  (esh-header "\n┌─")
  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (eshell-prompt-regexp "└─> ")
  (eshell-prompt-string "└─> ")
  ;; Separator between esh-sections
  (esh-sep " | ")
  ;; Separator between an esh-section icon and form
  (esh-section-delim " ")

  (eshell-banner-message (concat "Welcome to the\n"
                                 "           _          _ _ \n"
                                 "          | |        | | |\n"
                                 "  ___  ___| |__   ___| | |\n"
                                 " / _ \\/ __| '_ \\ / _ \\ | |\n"
                                 "|  __/\\__ \\ | | |  __/ | |\n"
                                 " \\___||___/_| |_|\\___|_|_|\n"
                                 "                          \n"
                                 "                          "))

  :config
  (require 'dash)
  (require 's)
  (require 'magit)
  (require 'all-the-icons)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (esh-section esh-dir
               (all-the-icons-faicon "folder") ; faicon folder icon: "\xf07c"
               (abbreviate-file-name (eshell/pwd))
               `(:foreground ,(doom-color 'yellow) :bold ultra-bold :underline t))

  (esh-section esh-git
               (all-the-icons-alltheicon "git") ; git icon: "\xe907"
               (magit-get-current-branch)
               `(:foreground ,(doom-color 'magenta)))

  (esh-section esh-clock
               (all-the-icons-faicon "clock-o") ; clock icon: "\xf017"
               (format-time-string "%H:%M" (current-time))
               `(:foreground ,(doom-color 'green)))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (+ esh-prompt-num 1))))

  (esh-section esh-num
               (all-the-icons-faicon "list") ; list icon: "\xf0c9"
               (number-to-string esh-prompt-num)
               `(:foreground ,(doom-color 'orange)))

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func))

(provide 'interface)

;;; interface.el ends here
