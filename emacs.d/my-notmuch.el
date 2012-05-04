; My notmuch configurations

(when (require 'notmuch nil t)
  (setq message-kill-buffer-on-exit 1)
  (setq user-mail-address (notmuch-user-primary-email)
        user-full-name (notmuch-user-name))

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-saved-searches
        '(("inbox" . "tag:inbox")
          ("unread" . "tag:unread")
          ("jyu" . "tag:jyu")
          ("linkki" . "tag:linkki")
          ("gmail" . "tag:gmail")
          ("rss" . "tag:rss AND tag:unread")))

  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread))
