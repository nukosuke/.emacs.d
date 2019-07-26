;;; counsel-dispatch.el --- Counsel transient commands -*- lexical-binding: t; -*-
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
;;  * 2019/03/06:
;;    Create counsel-dispatch.el
;;    - Define transient commands for counsel

;;; Code:

(require 'transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x counsel-dispatch
;; transient commands for counsel and its extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-transient-command counsel-dispatch ()
  "Invoke a Counsel command from a list of available commands."
  ["Transient commands\n"

   ["Counsel"
    ("a" "ag"       counsel-ag)
    ("b" "Bookmark" counsel-bookmark)]

   ["Git" ;; TODO check if current directory is a git worktree
    ("g" "Find file"       counsel-git)
    ("c" "Checkout branch" counsel-git-checkout)
    ("l" "Log"             counsel-git-log)
    ("w" "Change worktree" counsel-git-change-worktree)]

   ["Projectile"
    ("A" "ag"             counsel-projectile-ag)
    ("f" "Find file"      counsel-projectile-find-file)
    ("d" "Find directory" counsel-projectile-find-dir)
    ("p" "Switch project" counsel-projectile-switch-project)]

   ["Tabs"
    ("G" "Switch tab group" centaur-tabs-counsel-switch-group)]

   ["Org" ;; TODO: only from org-mode
    ("o" "Goto outline" counsel-org-goto)]])

(provide 'counsel-dispatch)

;;; counsel-dispatch.el ends here
