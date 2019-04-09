;;; straight-dispatch.el --- straight.el transient commands -*- lexical-binding: t; -*-
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
;;    Create straight-dispatch.el
;;    - Define transient commands for straight.el

;;; Code:

(use-package transient
  :bind
  ("C-c p" . straight-dispatch)

  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; M-x straight-dispatch
  ;; transient version of hydra-straight-helper
  ;;   https://github.com/abo-abo/hydra/wiki/straight.el
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-transient-command straight-dispatch ()
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
     ("e" "Prune build"        straight-prune-build)]))

(provide 'straight-dispatch)

;;; straight-dispatch.el ends here
