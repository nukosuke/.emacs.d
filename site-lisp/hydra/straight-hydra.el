;;; hydra-straight.el --- Hydra for straight.el -*- lexical-binding: t; -*-
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
;;
;;  * 2019/08/22:
;;    Migrate from transient.el to hydra

;;; Code:

(use-package lv)
(use-package hydra
  :after lv
  :config
  (defhydra hydra-straight (:hint nil)
    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil))

  :bind
  ("C-c p" . hydra-straight/body))

(provide 'hydra/straight)

;; hydra-straight.el ends here
