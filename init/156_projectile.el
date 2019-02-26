;;; 156_projectile.el -- config for projectile
;;; Commentary:
;; Projectileの設定
;; - グローバルで有効
;; - 検索にhelmを使用



(use-package projectile
  :init
  (projectile-global-mode)
  :custom
  (projectile-completion-system 'helm)
  :diminish)

(provide '156_projectile)
;;; 156_projectile.el ends here
