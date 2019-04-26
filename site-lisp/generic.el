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
 ;; FIXME: emacs-version includes build number from Emacs 27.0.50
 ;;   Want just only "GNU Emacs <major>.<minor>.<patch>"
 '(frame-title-format           (emacs-version))
 '(indent-tabs-mode             nil)
 '(make-backup-files            nil)
 '(scroll-step                  1)
 '(tab-width                    2)
 '(x-select-enable-clipboard    t)
 ;; For auto refresh branch name of modeline.
 ;; Don't set this in particular package :custom section because
 ;; branch can be changed at many situations.
 '(auto-revert-check-vc-info    t)
 ;; Org customs must be set here.
 ;; Because it cannot be overwritten by use-package (Bug or design).
 '(org-agenda-files             '("~/.emacs.d/org/"))
 '(org-return-follows-link      t)
 '(org-use-speed-commands       t))

;;
;; Font settings
;;
(if (memq window-system '(mac ns))
    (let* ((font-family "Menlo")
           (font-size 12)
           (font-height (* font-size 10))
           (jp-font-family "ヒラギノ角ゴ ProN"))
      (set-face-attribute 'default nil :family font-family :height font-height)
      (let ((name (frame-parameter nil 'font))
            (jp-font-spec (font-spec :family jp-font-family))
            (jp-characters '(katakana-jisx0201
                             cp932-2-byte
                             japanese-jisx0212
                             japanese-jisx0213-2
                             japanese-jisx0213.2004-1))
            (font-spec (font-spec :family font-family))
            (characters '((?\u00A0 . ?\u00FF)    ; Latin-1
                          (?\u0100 . ?\u017F)    ; Latin Extended-A
                          (?\u0180 . ?\u024F)    ; Latin Extended-B
                          (?\u0250 . ?\u02AF)    ; IPA Extensions
                          (?\u0370 . ?\u03FF)))) ; Greek and Coptic
        (dolist (jp-character jp-characters)
          (set-fontset-font name jp-character jp-font-spec))
        (dolist (character characters)
          (set-fontset-font name character font-spec))
        (add-to-list 'face-font-rescale-alist (cons jp-font-family 1.2)))))

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

(bind-key "C-h" 'delete-backward-char)
(bind-key "C-m" 'newline-and-indent)
(bind-key "C-z" 'winner-undo)
(bind-key "M-m" 'set-mark-command)

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
