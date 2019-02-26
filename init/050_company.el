;;; 050_company.el -- 補完機能設定 -*- lexical-binding: t -*-
;;; Commentary:
;;
;; companyによる補完設定
;; 候補表示にはcompany-boxを使う
;;
;;; Code:

;;
;; company全般
;;
(use-package company
  :init
  (global-company-mode)
  ;; 下までいったら上に戻ってくる（逆も同じ）
  (setq company-selection-wrap-around t)

  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-h" . nil))

  :diminish)

;;
;; company-box
;;
;; 表示崩れのためdisabled. 導入にはまだ早い.
;(use-package company-box
;  :if (version<= "26.1" emacs-version)
;  :straight (:host github :repo "sebastiencs/company-box")
;  :hook (company-mode . company-box-mode)
;  :diminish)

;;
;; Language Server Protocolを補完に使用
;;
(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends))

(provide '050_company)

;;; 050_company.el ends here
