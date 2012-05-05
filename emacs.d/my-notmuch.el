; My notmuch configurations

(when (require 'notmuch nil t)
  (setq message-kill-buffer-on-exit 1)
  (setq user-mail-address (notmuch-user-primary-email)
        user-full-name (notmuch-user-name))
  (setq notmuch-message-headers '("Subject" "To" "From" "Cc" "Date"))
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-saved-searches
        '(("inbox" . "tag:inbox AND tag:unread")
          ("unread" . "tag:unread")
          ("jyu" . "tag:jyu AND tag:unread")
          ("linkki" . "tag:linkki AND tag:unread")
          ("gmail" . "tag:gmail AND tag:unread")
          ("rss" . "tag:rss AND tag:unread")
          ("facebook" . "tag:facebook AND tag:unread")))

  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)

  (when (require 'notmuch-address nil t)
    (setq notmuch-address-command (concat (getenv "HOME") "/bin/nottoomuch-addresses.sh"))
    (notmuch-address-message-insinuate)))
