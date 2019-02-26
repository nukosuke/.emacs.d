;;; 001_general.el -- 共通の設定
;;; Commentary:
;;; Code:
(custom-set-variables
 '(current-language-environment "UTF-8")
 ;; カーソルホバー時に括弧の対応を表示する
 '(show-paren-mode t)
 ;; タイトルバーフォーマット
 '(frame-title-format (format "%%f" (system-name)))
 ;; バックアップファイル(*~)を作成しない
 '(make-backup-files nil)
 ;; タブ幅を4に指定
 '(tab-width 2)
 ;; タブをスペースに
 '(indent-tabs-mode nil)
 ;; kill & yankにクリップボードを使う
 '(x-select-enable-clipboard t))

;; ツールバー非表示
(tool-bar-mode 0)

;; メニューバー非表示
(menu-bar-mode 0)

;; スクロールバー非表示
(scroll-bar-mode 0)

;; スクロールを1行ずつ
(setq scroll-step 1)

;; ピクセル単位のスクロールを有効化
(pixel-scroll-mode 1)

;; macOSでCommandキーをmetaに割り当て
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; use-package :diminish :delight
(use-package diminish)
(use-package delight)

;; transient
;; is also in magit
(use-package transient)

;; straight.el helper
;; transient version of https://github.com/abo-abo/hydra/wiki/straight.el
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
   ("e" "Prune build"        straight-prune-build)])

;; Todo
;; - move to other file
(define-transient-command zoom-dispatch ()
  "Invoke a zoom command from a list of available commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Zoom commands"
   ("g" "Zoom IN" text-scale-increase)
   ("l" "Zoom OUT" text-scale-decrease)])
(global-set-key (kbd "<f2>") 'zoom-dispatch)

(provide '001_general)
;;; 001_general.el ends here
