;; -*- lexical-binding: t; -*-
;;;; My notmuch configurations

(require 'notmuch)

;;;; Functions
(defun notmuch-show-browser-browse-part (message-id nth &optional filename content-type)
  "Open all the HTML attachments in a browser. Save all the other attachments."
  (if (string= content-type "text/html")
      (notmuch-with-temp-part-buffer message-id nth
                                     (set-buffer-file-coding-system 'raw-text)
                                     (browse-url-of-buffer))
    (notmuch-show-save-part message-id nth filename content-type)))

(defun notmuch-show-toggle-tag (tag)
  "Toggles the provided tag for the current message."
  (notmuch-show-tag-message
   (if (member tag (notmuch-show-get-tags))
       (concat "-" tag) (concat "+" tag))))

(defun notmuch-search-toggle-tag (tag)
  "Toggles the provided tag for the current message."
  (notmuch-search-tag
   (if (member tag (notmuch-search-get-tags))
       (concat "-" tag) (concat "+" tag))))

(defun notmuch-show-subject-tabs-to-spaces ()
  "Replace tabs with spaces in subject line."
  (goto-char (point-min))
  (when (re-search-forward "^Subject:" nil t)
    (while (re-search-forward "\t" (line-end-position) t)
      (replace-match " " nil nil))))

;;;; Commands
(defun notmuch-show-part-button-browser-browse (&optional button)
  (interactive)
  (notmuch-show-part-button-internal button #'notmuch-show-browser-browse-part))

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

(defun notmuch-quick-search (key)
  "Quick search."
  (interactive "kQuick search:")
  (notmuch-search (cdr (assoc key notmuch-quick-search-alist))))

(defun notmuch-show-np-or-nt ()
  "Shows the next page or advances to next thread."
  (interactive)
  (if (notmuch-show-advance)
      (notmuch-show-next-thread t)))

(defun notmuch-show-nt ()
  "Advance to the next thread."
  (interactive)
  (notmuch-show-next-thread t))

(defun notmuch-show-pt ()
  "Go back to the previous thread."
  (interactive)
  (notmuch-show-previous-thread t))

;;;; Settings
(setq message-kill-buffer-on-exit 1
      user-mail-address (notmuch-user-primary-email)
      user-full-name (notmuch-user-name)
      notmuch-message-headers '("Subject" "To" "Cc" "Date")
      notmuch-search-oldest-first nil
      notmuch-search-oldest-first nil
      notmuch-show-all-multipart/alternative-parts nil
      notmuch-show-part-button-default-action 'notmuch-show-browser-browse-part
      notmuch-search-line-faces nil)
(setq notmuch-saved-searches
      '(("inbox" . "tag:inbox")
        ("unread" . "tag:unread")
        ("flagged" . "tag:flagged")
        ("mlists" . "tag:mlists")
        ("jyu" . "tag:jyu")
        ("linkki" . "tag:linkki")
        ("gmail" . "tag:gmail")
        ("rss" . "tag:rss")
        ("unresolved" . "tag:unresolved")))
(setq notmuch-hello-sections
      '(notmuch-hello-insert-header
        notmuch-hello-insert-saved-searches
        notmuch-hello-insert-recent-searches
        notmuch-hello-insert-alltags
        notmuch-hello-insert-footer))
(setq gnus-inhibit-images t
      mm-text-html-renderer 'w3m
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(defvar notmuch-quick-search-alist
  '(("z" . "tag:inbox or tag:unread or tag:flagged")
    ("i" . "tag:inbox")
    ("u" . "tag:unread")
    ("f" . "tag:flagged")
    ("l" . "tag:mlists")
    ("x" . "tag:unresolved")
    ("j" . "tag:jyu")
    ("r" . "tag:rss")
    ("m" . "tag:me")
    ("a" . "*"))
  "Notmuch quick search items.")

(defvar notmuch-tag-toggle-key-list
  '(("d" . "deleted")
    ("," . "unread")
    ("." . "flagged")
    (";" . "inbox"))
  "Notmuch tag key items.")

;;;; Keybindings

;; hello
(define-key notmuch-hello-mode-map "z" 'notmuch-quick-search)

;; search
(define-key notmuch-search-mode-map "z" 'notmuch-quick-search)
(define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
(define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)

;; show
(define-key notmuch-show-mode-map "z" 'notmuch-quick-search)
(define-key notmuch-show-mode-map "n" 'notmuch-show-next-message)
(define-key notmuch-show-mode-map "p" 'notmuch-show-previous-message)
(define-key notmuch-show-mode-map "N" 'notmuch-show-next-open-message)
(define-key notmuch-show-mode-map "P" 'notmuch-show-previous-open-message)
(define-key notmuch-show-mode-map " " 'notmuch-show-np-or-nt)
(define-key notmuch-show-mode-map (kbd "M-n") 'notmuch-show-nt)
(define-key notmuch-show-mode-map (kbd "M-p") 'notmuch-show-pt)

;; tag toggling keys
(mapc
 (lambda (pair)
   (let ((key (car pair))
         (tag (cdr pair)))
     (define-key notmuch-show-mode-map key
       (lambda ()
         "Toggle a tag for current message."
         (interactive)
         (notmuch-show-toggle-tag tag)))
     (define-key notmuch-search-mode-map key
       (lambda ()
         "Toggle a tag for selected message."
         (interactive)
         (notmuch-search-toggle-tag tag)))))
 notmuch-tag-toggle-key-list)

;;;; Notmuch-address
(when (require 'notmuch-address nil t)
  (setq notmuch-address-command (concat (getenv "HOME") "/bin/nottoomuch-addresses.sh"))
  (notmuch-address-message-insinuate))

;;;; Hooks

(add-hook 'notmuch-show-markup-headers-hook 'notmuch-show-subject-tabs-to-spaces)
