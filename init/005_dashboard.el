;;; 005_dashboard.el -- 起動時ダッシュボード
;;; Commentary:
;;; Code:

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title (concat "GNU Emacs " emacs-version))
  :config
  (dashboard-setup-startup-hook))

(provide '005_dashboard)
;;; 005_dashboard ends here
