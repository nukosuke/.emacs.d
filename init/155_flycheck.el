;;; 155_flycheck.el -- 文法チェック設定
;;; Commentary:
;;; Code:

(use-package flycheck
  :after git-gutter
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X...."))

(use-package flycheck-pos-tip
  :after flycheck
  :custom
  (flycheck-display-errors-function #'flycheck-pos-tip-error-messages "エコーエリアではなくtipに表示する"))

(provide '155_flycheck)
;;; 155_flycheck.el ends here
