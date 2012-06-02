;; My notmuch configurations

;; Functions
(defun notmuch-show-previous-thread (&optional show-next)
  "Move to the previous item in the search results, if any."
  (interactive "P")
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (notmuch-search-previous-thread)
      (if show-next
          (notmuch-search-show-thread)))))
(defun notmuch-show-browser-browse-part (message-id nth &optional filename content-type)
  "Open all the HTML attachments in a browser. Save all the other attachments."
  (if (string= content-type "text/html")
      (notmuch-with-temp-part-buffer message-id nth
                                     (set-buffer-file-coding-system 'raw-text)
                                     (browse-url-of-buffer))
    (notmuch-show-save-part message-id nth filename content-type)))
(defun notmuch-show-part-button-browser-browse (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-browser-browse-part))

(when (require 'notmuch nil t)
  ;; Settings
  (setq message-kill-buffer-on-exit 1
        user-mail-address (notmuch-user-primary-email)
        user-full-name (notmuch-user-name)
        notmuch-message-headers '("Subject" "To" "Cc" "Date")
        notmuch-search-oldest-first nil
        notmuch-search-oldest-first nil
        notmuch-show-all-multipart/alternative-parts nil
        notmuch-show-part-button-default-action 'notmuch-show-browser-browse-part)
  (setq notmuch-saved-searches
        '(("inbox" . "tag:inbox")
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
  (setq gnus-inhibit-images t
        mm-text-html-renderer 'w3m)

  ;; Keybindings
  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)
  (define-key notmuch-show-mode-map "d"
    (lambda ()
      "Toggles deleted tag for the current message"
      (interactive)
      (notmuch-show-tag-message
       (if (member "deleted" (notmuch-show-get-tags))
           "-deleted" "+deleted"))))
  (define-key notmuch-search-mode-map "d"
    (lambda ()
      "Toggles deleted tag for the current message"
      (interactive)
      (notmuch-search-tag
       (if (member "deleted" (notmuch-search-get-tags))
           "-deleted" "+deleted"))))
  (define-key notmuch-show-mode-map " "
    (lambda ()
      "Shows the next page or advances to next thread."
      (interactive)
      (if (notmuch-show-advance)
          (notmuch-show-next-thread t))))
  (define-key notmuch-show-mode-map (kbd "M-n")
    (lambda ()
      "Advance to the next thread."
      (interactive)
      (notmuch-show-next-thread t)))
  (define-key notmuch-show-mode-map (kbd "M-p")
    (lambda ()
      "Go back to the previous thread."
      (interactive)
      (notmuch-show-previous-thread t)))

  ;; Notmuch-address
  (when (require 'notmuch-address nil t)
    (setq notmuch-address-command (concat (getenv "HOME") "/bin/nottoomuch-addresses.sh"))
    (notmuch-address-message-insinuate)))
