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
  (ivy-height                   20))

(use-package ivy-rich
  :after counsel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel: a collection of Ivy-enhanced versions
;;          of common Emacs commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :hook
  (after-init . ivy-mode)
  (after-init . ivy-rich-mode)

  :bind
  ("C-s"     . swiper)
  ("C-x b"   . ivy-switch-buffer)
  ("M-x"     . counsel-M-x)
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
        ("C-p" . company-select-previous)))

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
         (conf-mode . smartparens-mode)))

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
