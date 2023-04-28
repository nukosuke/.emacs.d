;;; social.el --- SNS config -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022- nukosuke.
;;
;; Author:  nukosuke <nukosuke@lavabit.com>
;; URL:     https://github.com/nukosuke/.emacs.d
;; License: GPLv3+
;;
;;; Commentary:
;;
;;  These files are NOT part of GNU Emacs.

;;; Code:

(use-package ement)

(use-package mastodon
  :straight (mastodon :type git :host codeberg :repo "martianh/mastodon.el")
  :custom
  (mastodon-instance-url     "https://mastodon.social")
  (mastodon-active-user      "nukosuke")
  (mastodon-tl--show-avatars t))

(provide 'social)

;;; social.el ends here
