;;; completion.el --- Completion settings -*- lexical-binding: t; -*-
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
;;    Create completion.el
;;    - Configure ivy/counsel
;;      https://github.com/abo-abo/swiper#ivy
;;      https://github.com/abo-abo/swiper#counsel
;;  * 2019/03/26:
;;    Start using ivy-rich
;;    - https://github.com/Yevgnen/ivy-rich

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy: a generic completion mechanism for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :custom
  (ivy-count-format             "[%d/%d] ")
  (ivy-use-virtual-buffers      t)
  (ivy-format-function          'ivy-format-function-arrow)
  (ivy-wrap                     t)
  (enable-recursive-minibuffers t)
  (ivy-height                   20)

  :config
  (ivy-mode 1)

  :diminish)

(use-package ivy-rich
  :after
  counsel

  :config
  ;; ivy-rich customize
  (defun ivy-rich-switch-buffer-icon (candidate)
    "Get icon from all-the-icons by CANDIDATE type."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (setq ivy-rich--display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-switch-buffer-icon :width 2)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x)
                                              (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand)
        (get-buffer cand)))
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer (:width 40))  ; the original transformer
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
     counsel-recentf
     (:columns
      ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
       ; return the last modified time of the file
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

;  :config
  (ivy-rich-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel: a collection of Ivy-enhanced versions
;;          of common Emacs commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x b"   . counsel-switch-buffer)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-c l"   . counsel-dispatch) ;; Defined at dispatcher/counsel-dispatch.el
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(use-package counsel-projectile
  :after counsel

  :bind
  ("C-x B" . counsel-projectile-switch-to-buffer)
  ("C-x f" . counsel-projectile-find-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company: Modular in-buffer completion framework
;;          for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :custom
  (company-selection-wrap-around t)

  :hook
  ((text-mode . company-mode)
   (prog-mode . company-mode)
   (conf-mode . company-mode))

  :bind
  (:map company-active-map
        ("C-h" . nil)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))

  :diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP mode: Language Server Protocol support for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands lsp)

(use-package company-lsp
  :commands company-lsp)

(use-package company-quickhelp
  :after company
  :hook
  (company-mode . company-quickhelp-mode))

;; TODO: company-box
;;       But, now posframe has problem.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens: Minor mode for Emacs that deals with
;;              parens pairs and tries to be smart
;;              about it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  ;; Enable after open file.
  :hook ((text-mode . smartparens-mode)
         (prog-mode . smartparens-mode)
         (conf-mode . smartparens-mode))

  :diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet: A template system for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: keybind
(use-package yasnippet
  :custom
  (yas-snippet-dirs
   '("~/.emacs.d/snippets"
     "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))

  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(provide 'completion)

;;; completion.el ends here
