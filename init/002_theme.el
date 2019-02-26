;;; 002_theme.el -- テーマの設定
;;; Commentary:
;;
;; doom-themes:
;; - 全体テーマ
;;
;; powerline:
;; - モードラインテーマ
;;
;;; Code:

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package powerline
  :init
  (powerline-default-theme))

(provide '002_theme)

;;; 002_theme.el ends here
