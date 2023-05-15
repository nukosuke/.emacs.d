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
;;
;;  * 2019/03/26:
;;    Start using ivy-rich
;;
;;  * 2022/11/15:
;;    Migrate completion system
;;    - from company-mode to corfu
;;    - from ivy/counsel/swiper to vertico/orderless/consult

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico.el - VERTical Interactive COmpletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :custom
  (vertico-cycle t)

  :init
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  :load-path "straight/repos/vertico/extensions"

  :bind
  (:map vertico-map
        ("C-j"   . vertico-directory-enter)
        ("C-h"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orderless
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marginalia.el - Marginalia in the minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)

  :init
  (nerd-icons-completion-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consult.el - Consulting completing-read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :bind
  (("M-y"     . consult-yank-pop)
   ("C-c l"   . consult-dispatch)
   ("C-c m"   . consult-mode-command)
   ("C-c k"   . consult-kmacro)
   ("C-x b"   . consult-buffer)
   ("C-c i"   . consult-imenu)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ("C-x C-r" . consult-recent-file)
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))

  :config
  (consult-customize
   consult-buffer :preview-key "M-."
   consult-line :prompt "Search: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; corfu.el: Completion Overlay Region FUnction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto  t)
  (corfu-preview-current    nil)

  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :load-path "straight/repos/corfu/extensions"
  :config
  (corfu-popupinfo-mode))

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

(use-package consult-yasnippet
  :after (yasnippet consult))

(provide 'completion)

;;; completion.el ends here
