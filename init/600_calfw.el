;;; 600_calfw.el -- Rich calendar
;;; Commentary:
;; calfw & calfw-org の設定

(use-package calfw)
(use-package calfw-org
  :after (calfw org)
  :custom
  (cfw:org-overwrite-default-keybinding t "Orgmode like key binding"))

(provide '600_calfw)

;;; 600_calfw.el ends here
