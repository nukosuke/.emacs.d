;;; 003_exec-path-from-shell.el -- PATH変数をシェルから読み込む設定
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :init
  ;; .zshrcにPATH設定しているときのメッセージを表示しない
  (setq exec-path-from-shell-check-startup-files nil)

  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; 003_exec-path-from-shell.el ends here
