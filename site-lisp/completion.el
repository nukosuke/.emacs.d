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

;; (use-package ivy-posframe
;;   :requires ivy-posframe-mode
;;   :init
;;   (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-posframe-display-at-point)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display))))

(use-package ivy-rich
  :after counsel)

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

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
  ("C-c i"   . counsel-imenu)
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
  (company-minimum-prefix-length 2)

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

;; NOTE: macOS has fullscreen problem with childframe
;;       until the commit bellow would be merged.
;;       https://emba.gnu.org/emacs/emacs/-/commit/bbc48b263485c26c6823eabdbbd7e9af62178e34
;;
;; NOTE: company-box is alternative, but it seems abandoned project
(use-package company-posframe
  :after company
  :hook (company-mode . company-posframe-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens: Minor mode for Emacs that deals with
;;              parens pairs and tries to be smart
;;              about it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :config
  (require 'smartparens-config)

  ;; Enable after open file.
  :hook ((text-mode . smartparens-mode)
         (prog-mode . smartparens-mode)
         (conf-mode . smartparens-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet: A template system for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: create transient command and bind key
(use-package yasnippet
  ;; Load path of snippets/ dir before YASnippet itself
  :after yasnippet-snippets

  :custom
  (yas-snippet-dirs
   `(,(concat user-emacs-directory "snippets")
     ,(concat user-emacs-directory "straight/repos/yasnippet-snippets/snippets")))

  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(provide 'completion)

;;; completion.el ends here
