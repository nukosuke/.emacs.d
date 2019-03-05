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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy: a generic completion mechanism for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :config
  (ivy-mode                          1)
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-wrap                     t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height                   20)

  :diminish)

(use-package ivy-rich
  :after ivy

  :custom
  (ivy-format-function #'ivy-format-function-line)

  :config
  (ivy-rich-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel: a collection of Ivy-enhanced versions
;;          of common Emacs commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

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

(provide 'completion)

;;; completion.el ends here
