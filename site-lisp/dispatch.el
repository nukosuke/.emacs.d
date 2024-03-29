;;; dispatch.el --- transient commands -*- lexical-binding: t; -*-
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
;;    Create zoom-dispatch.el
;;    - Define transient command to zoom in/out window.
;;
;;    Create straight-dispatch.el
;;    - Define transient commands for straight.el
;;
;;  * 2019/03/06:
;;    Create counsel-dispatch.el
;;    - Define transient commands for counsel
;;
;;    Create avy-dispatch.el
;;    - Define transient commands for avy
;;
;;  * 2019/04/09:
;;    Create git-gutter-dispatch.el
;;    - Define transient commands for git-gutter
;;
;;  * 2019/05/13:
;;    Add resize-dispatch
;;
;;  * 2021/06/09:
;;    Unify *-dispatch.el into dispatch.el
;;
;;  * 2022/11/07:
;;    Replace counsel-ag to counsel-rg

;;; Code:

(require 'transient)
(require 'bind-key)

(global-set-key (kbd "C-c a") 'avy-dispatch)
(global-set-key (kbd "C-c p") 'straight-dispatch)
(global-set-key (kbd "C-c r") 'resize-dispatch)
(global-set-key (kbd "<f9>") 'zoom-dispatch)

(transient-define-prefix avy-dispatch ()
  "Invoke a Avy command from a list of available commands."
  ["Avy commands\n"
   ["Line"
    ("y" "Yank" avy-copy-line)
    ("m" "Move" avy-move-line)
    ("k" "Kill" avy-kill-whole-line)]

   ["Region"
    ("Y" "Yank" avy-copy-region)
    ("M" "Move" avy-move-region)
    ("K" "Kill" avy-kill-region)]

   ["Goto"
    ("c" "Timed char" avy-goto-char-timer)
    ("C" "Char"       avy-goto-char)
    ("w" "Word"       avy-goto-word-1)
    ("W" "Any word"   avy-goto-word-0)
    ("l" "Line"       avy-goto-line)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x consult-dispatch
;; transient commands for consult and its extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-prefix consult-dispatch ()
  "Invoke a Consult command from a list of available commands."
  ["Transient commands\n"
   ["Consult"
    ("f" "Find file" consult-find)
    ("g" "Git grep"  consult-git-grep)
    ("r" "Ripgrep"   consult-ripgrep)
    ("b" "Bookmark"  consult-bookmark)]
   ["Tabs"
    ("G" "Switch tab group" centaur-tabs-switch-group)]
   ;["Dash"
   ; ("D" "Query docset"   counsel-dash)
   ; ("i" "Install docset" counsel-dash-install-docset)]
   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-prefix project-dispatch ()
  "Invoke a project command from a list of available commands."
  ["Project\n"
   ["File/Dir"
    ("f" "Find file"      project-find-file)
    ("d" "Find directory" project-find-dir)
    ("D" "Dired"          project-dired)]
   ["Switch"
    ("b" "Switch to buffer" project-switch-to-buffer)
    ("s" "Switch project"   project-switch-project)]
   ["Shell"
    ("!" "Shell command"       project-shell-command)
    ("&" "Async shell command" project-async-shell-command)
    ("e" "Eshell"              project-eshell)]])

(bind-key* "C-x p" 'project-dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x straight-dispatch
;; transient version of hydra-straight-helper
;;   https://github.com/abo-abo/hydra/wiki/straight.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-prefix straight-dispatch ()
  "Invoke a straight.el command from a list of available commands."
  ["straight.el commands\n"
   ["Check"
    ("c" "Check all"         straight-check-all)
    ("C" "Check package"     straight-check-package)]
   ["Fetch"
    ("f" "Fetch all"         straight-fetch-all)
    ("F" "Fetch package"     straight-fetch-package)]
   ["Merge"
    ("m" "Merge all"         straight-merge-all)
    ("M" "Merge package"     straight-merge-package)]
   ["Normalize"
    ("n" "Normalize all"     straight-normalize-all)
    ("N" "Normalize package" straight-normalize-package)]
   ["Push"
    ("u" "Push all"          straight-push-all)
    ("U" "Push package"      straight-push-package)]]
  [["Rebuild"
    ("r" "Rebuild all"       straight-rebuild-all)
    ("R" "Rebuild package"   straight-rebuild-package)]
   ["Pull"
    ("p" "Pull all"          straight-pull-all)
    ("P" "Pull package"      straight-pull-package)]
   ["Version"
    ("v" "Versions freeze"   straight-freeze-versions)
    ("V" "Versions thaw"     straight-thaw-versions)]
   ["Watcher"
    ("w" "Watcher start"     straight-watcher-start)
    ("W" "Watcher stop"      straight-watcher-stop)]]
  ["Other"
   ("g" "Get recipe"         straight-get-recipe)
   ("e" "Prune build"        straight-prune-build)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x zoom-dispatch
;; g : Zoom IN
;; l : Zoom OUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-prefix zoom-dispatch ()
  "Invoke a zoom command from a list of available commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Zoom commands"
   ("g" "Zoom IN" text-scale-increase)
   ("l" "Zoom OUT" text-scale-decrease)])

(transient-define-prefix resize-dispatch ()
  "Invoke a resize command from a list of available commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Resize commands"
   ("{" "Shrink window horizontally" shrink-window-horizontally)
   ("}" "Enlarge window horizontally" enlarge-window-horizontally)])

(provide 'dispatch)

;;; dispatch.el ends here
