;;; popup-init.el -- popupの初期化
;;; Commentary:
;;; Code:

(use-package popup
  :bind
  (:map popup-menu-keymap
        ;; TabもしくはCtrl+nで次の候補に移動
        ("TAB" . popup-next)
        ("C-n" . popup-next)
        ;; Shift+TabもしくはCtrl+pで前の候補に移動
        ("<backtab>" . popup-previous)
        ("C-p" . popup-previous)))

(provide 'popup-init)
;;; popup-init.el ends here
