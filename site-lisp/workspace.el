;;; workspace.el --- Workspace settings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  These files are NOT part of GNU Emacs.
;;
;;  * 2019/02/27:
;;    Create workspace.el
;;    - Configure workspace extensions

;;; Code:

(use-package neotree)

(use-package projectile
  :custom
  projectile-completion-system 'ivy
  :bind
  (:map projectile-mode-map
        ("C-x p" . projectile-switch-project)) ;; FIXME: dispatch-projectile (transient)
  :config
  (projectile-mode 1))

(provide 'workspace)

;;; workspace.el ends here
