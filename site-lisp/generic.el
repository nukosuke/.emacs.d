;;; generic.el --- General settings -*- lexical-binding: t; -*-
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
;;    Create generic.el
;;    - Configure custom-set-variables

;;; Code:

(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; Variable                    Value
 '(current-language-environment "UTF-8")
 '(frame-title-format           (format "GNU Emacs %s" emacs-version))
 '(indent-tabs-mode             nil)
 '(make-backup-files            nil)
 '(scroll-step                  1)
 '(tab-width                    2)
 '(x-select-enable-clipboard    t)
 '(ring-bell-function           'ignore)
 ;; For auto refresh branch name of modeline.
 ;; Don't set this in particular package :custom section because
 ;; branch can be changed at many situations.
 '(auto-revert-check-vc-info    t)
 ;; Org customs must be set here.
 ;; Because it cannot be overwritten by use-package (Bug or design).
 '(org-agenda-files             `(,(concat user-emacs-directory "org/")))
 '(org-return-follows-link      t)
 '(org-use-speed-commands       t)
 '(use-short-answers            t))

;;
;; Font settings
;;
(if (memq window-system '(mac ns))
    (let* ((size 15)
           (ascii-font "PlemolJP")
           (jp-font "PlemolJP")
           (font-height (* size 10))
           (font-spec (font-spec :family ascii-font))
           (jp-font-spec (font-spec :family jp-font)))
      (set-face-attribute 'default nil :family ascii-font :height font-height)
      (set-fontset-font nil 'japanese-jisx0212 jp-font-spec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-font-spec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-font-spec)
      (set-fontset-font nil '(#x0080 . #x024F) font-spec)
      (set-fontset-font nil '(#x0370 . #x03FF) font-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import $PATH from shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :if
  (memq window-system '(mac ns x))

  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; macOS specific settings
;;
(when (eq system-type 'darwin)
  ;; Use <command> as meta key
  (setq ns-command-modifier (quote meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(bind-key "C-h" 'delete-backward-char) ;; see puni bind
(bind-key "C-m" 'newline-and-indent)
(bind-key "C-z" 'winner-undo)
(bind-key "M-m" 'set-mark-command)
(bind-key "M-p" 'scroll-down)
(bind-key "M-n" 'scroll-up)

;; built-in project.el bindings
(use-package project
  :straight nil
  :bind
  ("C-x B" . project-switch-to-buffer)
  ("C-x f" . project-find-file))

;; Use ibuffer to manage buffers and turn off confirmation for
;; destructive operation.
(use-package ibuffer
  :straight nil
  :custom (ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequential commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sequential-command
  :init
  (require 'sequential-command-config)

  :bind
  ;; current position
  ;; -> beginning of line
  ;; -> beginning of buffer
  ;; -> return to initial position ^
  ("C-a" . seq-home)

  ;; current position
  ;; -> end of line
  ;; -> end of buffer
  ;; -> return to initial position ^
  ("C-e" . seq-end)

  ;; Org specific
  (:map org-mode-map
        ("C-a" . org-seq-home)
        ("C-e" . org-seq-end)))

(provide 'generic)

;;; generic.el ends here
