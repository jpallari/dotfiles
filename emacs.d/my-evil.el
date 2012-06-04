;; My EVIL config

(when (require 'evil nil t)
  (setq evil-default-state 'insert
        evil-move-cursor-back nil
        evil-want-fine-undo t)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "C-t") 'other-window)
  (define-key evil-normal-state-map (kbd "-") 'evil-window-decrease-height)
  (define-key evil-normal-state-map (kbd "+") 'evil-window-increase-height)

  ;; emacs mode as insert mode
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-normal-state-map (kbd "C-h") 'help-command)
  (define-key evil-insert-state-map (kbd "C-z") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-z") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-z") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-x C-a") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-x C-a") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-x C-a") 'evil-normal-state)

  ;; org-mode mappings
  (evil-declare-key 'normal org-mode-map
                    (kbd "TAB") 'org-cycle
                    (kbd "M-l") 'org-metaright
                    (kbd "M-h") 'org-metaleft
                    (kbd "M-k") 'org-metaup
                    (kbd "M-j") 'org-metadown
                    (kbd "M-L") 'org-shiftmetaright
                    (kbd "M-H") 'org-shiftmetaleft
                    (kbd "M-K") 'org-shiftmetaup
                    (kbd "M-J") 'org-shiftmetadown)

  ;; evil surround
  (when (require 'surround nil t)
    (global-surround-mode 1)))
