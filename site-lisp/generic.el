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
 '(org-return-follows-link      t)
 '(org-use-speed-commands       t))

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

;; SKK
;; TODO: input-method.elに移動

(use-package ddskk
  :commands
  skk-mode

  :bind
  ("C-\\" . skk-mode))

(provide 'generic)

;;; generic.el ends here
