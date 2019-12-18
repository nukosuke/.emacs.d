;;; prompt.el --- eshell custom prompt -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
               '(:foreground "gold" :bold ultra-bold :underline t))

  (esh-section esh-git
               (all-the-icons-alltheicon "git") ; git icon: "\xe907"
               (magit-get-current-branch)
               '(:foreground "pink"))

  (esh-section esh-clock
               (all-the-icons-faicon "clock-o") ; clock icon: "\xf017"
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (+ esh-prompt-num 1))))

  (esh-section esh-num
               (all-the-icons-faicon "list") ; list icon: "\xf0c9"
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func))

(provide 'eshell/prompt)

;;; prompt.el ends here
