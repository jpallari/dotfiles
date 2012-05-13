;; My notmuch configurations

(when (require 'notmuch nil t)
  ;; Functions
  (defun notmuch-show-browser-browse-part (message-id nth &optional filename content-type)
    (if (string= content-type "text/html")
        (notmuch-with-temp-part-buffer message-id nth
          (set-buffer-file-coding-system 'raw-text)
          (browse-url-of-buffer))
      (notmuch-show-save-part message-id nth filename content-type)))
  (defun notmuch-show-part-button-browser-browse (&optional button)
    (interactive)
    (notmuch-show-part-button-internal button #'notmuch-show-browser-browse-part))
  (setq notmuch-show-part-button-default-action 'notmuch-show-browser-browse-part)

  ;; Settings
  (setq message-kill-buffer-on-exit 1
        user-mail-address (notmuch-user-primary-email)
        user-full-name (notmuch-user-name)
        notmuch-message-headers '("Subject" "To" "Cc" "Date")
        notmuch-search-oldest-first nil
        notmuch-search-oldest-first nil
        notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-saved-searches
        '(("inbox" . "tag:inbox AND tag:unread")
          ("unread" . "tag:unread")
          ("jyu" . "tag:jyu AND tag:unread")
          ("linkki" . "tag:linkki AND tag:unread")
          ("gmail" . "tag:gmail AND tag:unread")
          ("rss" . "tag:rss AND tag:unread")
          ("facebook" . "tag:facebook AND tag:unread")))
  (setq notmuch-hello-sections
        '(notmuch-hello-insert-header
          notmuch-hello-insert-saved-searches
          notmuch-hello-insert-recent-searches
          notmuch-hello-insert-alltags
          notmuch-hello-insert-footer))

  ;; Keybindings
  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)
  (define-key notmuch-show-mode-map "d"
    (lambda ()
      (interactive)
      (notmuch-show-tag-message
       (if (member "deleted" (notmuch-show-get-tags))
           "-deleted" "+deleted"))))
  (define-key notmuch-search-mode-map "d"
    (lambda ()
      (interactive)
      (notmuch-search-tag
       (if (member "deleted" (notmuch-search-get-tags))
           "-deleted" "+deleted"))))

  ;; Notmuch-address
  (when (require 'notmuch-address nil t)
    (setq notmuch-address-command (concat (getenv "HOME") "/bin/nottoomuch-addresses.sh"))
    (notmuch-address-message-insinuate)))
