;;; 500_org.el -- config for Org
;;; Commentary:
;; Orgモードの設定全般

(use-package org
  ;; Emacs標準のバージョンを使う
  :straight nil

  :after (sequential-command
          magit) ;; NOTE: using transient in magit

  :custom
  ;; TODO: org-directory変更
  (org-return-follows-link t "RET will follow the link")
  (org-agenda-files '("~/Dropbox/emacs_sync/org/todo.org"
                      "~/Dropbox/emacs_sync/org/work.org") "Agenda files")
  (org-use-speed-commands t "Use speed commands")

  :bind
  ("C-c o". org-dispatch)
  (:map org-mode-map
        ("C-c C-v" . magit-dispatch)
        ("C-a" . seq-home)
        ("C-e" . seq-end)))

(define-transient-command org-dispatch ()
  "Invoke a Org-mode command from a list of available commands."
  ["Transient and dwim commands"
   [("a" "Agenda"     org-agenda)
    ("c" "Capture"    org-capture)
    ("l" "Store link" org-store-link)]
   [:if-derived org-mode
                ("e" "Export" org-export)]])

(define-transient-command org-export ()
  "Export Org document as/to other format."
  ["Export"
   [("m" "As Markdown" org-md-export-as-markdown)
    ("M" "To Markdown" org-md-export-to-markdown)]])

(provide '500_org)
;;; 500_org.el ends here
