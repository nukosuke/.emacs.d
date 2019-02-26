;;; 080_git.el -- gitの設定 -*- lexical-binding: t -*-
;;; Commentary:
;;
;; magit:
;; - gitクライアント
;;
;; git-gutter
;; - hunk単位の操作用
;;
;; git-gutter-fringe:
;; - 変更行にマークを表示
;;
;;; Code:

;;
;; magit
;;
(use-package magit
  :bind
  ("C-c C-v" . magit-dispatch))

;;
;; forge
;;
(use-package forge
  :bind
  ("C-c C-f" . forge-dispatch))

;;
;; git-gutter
;;
(use-package git-gutter
  :custom
  (git-gutter:ask-p nil)
  :bind
  ("C-c g" . git-gutter-dispatch))

(defun git-gutter:toggle-popup-hunk ()
  "Toggle git-gutter hunk window."
  (interactive)
  (if (window-live-p (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))

;;
;; git-gutterのhunk操作用transient-command
;;
(define-transient-command git-gutter-dispatch ()
  "Invoke a git-gutter command from a list of available commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["git hunk"
   ("p" "Previous" git-gutter:previous-hunk)
   ("n" "Next"     git-gutter:next-hunk)
   ("s" "Stage"    git-gutter:stage-hunk)
   ("r" "Revert"   git-gutter:revert-hunk)
   ("m" "Mark"     git-gutter:mark-hunk)
   ("SPC" "Toggle diffinfo" git-gutter:toggle-popup-hunk)])

;;
;; git-gutter-fringe
;;
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(provide '080_git)

;;; 080_git.el ends here
