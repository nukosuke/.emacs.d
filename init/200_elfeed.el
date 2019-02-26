;;; 200_elfeed.el -- RSS reader
;;; Commentary:
;;; Code:

(use-package elfeed

  :bind (("C-c r s" . elfeed))

  :config
  (setq elfeed-feeds
        '(("http://b.hatena.ne.jp/nukosuke/bookmark.rss" bookmark)
          ("http://b.hatena.ne.jp/nukosuke/unread_bookmark.rss" unread bookmark)
          ("http://b.hatena.ne.jp/nukosuke/hotentry.rss" hotentry)
          ("http://b.hatena.ne.jp/nukosuke/interest.rss" interest)
          ("https://emacs.cafe/feed.xml" blog emacs))))

;;; 200_elfeed.el ends here
