;;; 013_smart-mode-line.el --- make mode-line beautiful
;;; Commentary:
;;; Code:

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)

  :config
  (sml/setup))

(provide '013_smart-mode-line)
;;; 013_smart-mode-line.el ends here
