;;; 250_docker.el --- Docker管理

;;; Commentary:
;; dockerコマンドが存在するときのみ有効化する

;;; Code:

(use-package docker
  :if (executable-find "docker")
  :bind ("C-c d" . docker)
  :diminish)

;;; 250_docker.el ends here
