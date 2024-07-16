;;; term.el --- terminal configurations -*- lexical-binding: t -*-
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
;;  * 2022/11/09:
;;    Setup terminal config

;;; Code:

(require 'transient)

(transient-define-prefix term-dispatch ()
  "Dispatch terminal menu."
  ["Terminals"
   ["VTerm"
    ("v" "Create VTerm buffer" vterm)
    ("V" "Create multi VTerm buffer" multi-vterm)
    ("p" "Create multi VTerm in project root" multi-vterm-project)
    ]
   ["Eshell"
    ("e" "Create Eshell buffer" eshell)]
   ["ansi-term"
    ("a" "Create ansi-term buffer" ansi-term)]])

(bind-key "C-c t" 'term-dispatch)

;; remap shell-command to with-editor-shell-command
(bind-key "M-!" 'with-editor-shell-command)
(bind-key "M-&" 'with-editor-async-shell-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :hook
  (vterm-mode . with-editor-export-editor)

  :bind
  (:map vterm-mode-map
        ("C-h" . vterm-send-backspace)))

(use-package multi-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package term
  :straight (:type built-in)

  :hook
  (term-exec . with-editor-export-editor)

  :bind
  (:map term-raw-map
        ("C-c t" . nil)   ;; recover for ansi-term
        ("M-x"   . nil)   ;; recover for cousel-M-x
        ("M-o"   . nil))) ;; recover for ace-window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom eshell prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eshell
  :straight (:type built-in)
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

(provide 'term)

;;; term.el ends here
