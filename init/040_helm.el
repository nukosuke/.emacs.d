;;; 040_helm.el -- helmの設定
;;; Commentary:
;;
;; helmの設定
;;
;; helmでSilverSearcherを使う設定
;; C-c a g でagコマンドの実行結果をhelmで絞り込み
;;
;;; Code:

;;
;; helm
;;
(use-package helm
  :init
  (require 'helm-config)
  :custom
  ;; 下までいったら上に戻ってくる（逆も同じ）
  (helm-move-to-line-cycle-in-source t)
  :config
  ;; 常に有効化
  (helm-mode 1)
  :bind
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-x C-r" . helm-recentf)
  (:map helm-map
        ("C-h" . delete-backward-char))
  :diminish)

;;
;; helm-ag
;;
(use-package helm-ag
  :after helm
  :if (executable-find "ag")
  :bind ("C-c a g" . helm-ag))

(provide '040_helm)

;;; 040_helm.el ends here
