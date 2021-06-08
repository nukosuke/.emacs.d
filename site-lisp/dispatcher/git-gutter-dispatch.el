;;; git-gutter-dispatch.el --- git-gutter transient -*- lexical-binding: t; -*-
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
;;  * 2019/04/09:
;;    Create git-gutter-dispatch.el
;;    - Define transient commands for git-gutter

;;; Code:

(use-package transient
  :straight nil
  :ensure t
  :after magit

  :config
  (transient-append-suffix 'magit-dispatch "%" ;; change suffix if use forge
    '("g" "GitGutter" git-gutter-dispatch ?%))

  (transient-define-prefix git-gutter-dispatch ()
    "Invoke a git-gutter command from a list of available commands."
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["git-gutter hunk"
     ["Move"
      ("p"   "Prev"   git-gutter:previous-hunk)
      ("n"   "Next"   git-gutter:next-hunk)
      ("m"   "Mark"   git-gutter:mark-hunk)]
     ["Git"
      ("s"   "Stage"  git-gutter:stage-hunk)
      ("r"   "Revert" git-gutter:revert-hunk)
      ("SPC" "Popup"  git-gutter:popup-hunk)]]))

(provide 'git-gutter-dispatch)

;;; git-gutter-dispatch.el ends here
