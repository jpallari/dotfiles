; Common Lisp compatibility
(require 'cl)

; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

; Load paths
(add-to-list 'load-path' "~/.emacs.d/")
(when (> emacs-major-version 23)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

; Package management
(setq my-pkgs
  '(evil evil-leader auto-complete org
         haskell-mode jade-mode markdown-mode
         python stylus-mode undo-tree))
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(defun install-missing-packages ()
  "Installs all the missing packages"
  (interactive)
  (mapc
    (lambda (pkg)
      (or (package-installed-p pkg)
          (if (y-or-n-p (format "Package %s is missing. Install it? " pkg))
            (package-install pkg)))) my-pkgs))

; Keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c C-n") 'next-buffer)
(global-set-key (kbd "C-c C-p") 'previous-buffer)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)

; UI
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-background 'modeline "#0000ee")
(set-face-foreground 'modeline "#ffffff")
(set-cursor-color "#ffcc22")
(xterm-mouse-mode)
(set-mouse-color "#ffffff")

; Some defaults
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(turn-on-font-lock)

; Filetype specific settings
(load "~/.emacs.d/ft")

; Email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")

; Notmuch
(autoload 'notmuch "~/.emacs.d/my-notmuch" "notmuch mail" t)

; W3M
(require 'w3m-load nil t)
(require 'mime-w3m nil t)

; IDO
(require 'ido)

; EVIL
(when (require 'evil nil t)
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  ;(define-key evil-insert-state-map "\C-k" 'evil-delete-line)
  (define-key evil-normal-state-map "\C-h" 'evil-window-left)
  (define-key evil-normal-state-map "\C-j" 'evil-window-down)
  (define-key evil-normal-state-map "\C-k" 'evil-window-up)
  (define-key evil-normal-state-map "\C-l" 'evil-window-right)
  (define-key evil-normal-state-map "-" 'evil-window-decrease-height)
  (define-key evil-normal-state-map "+" 'evil-window-increase-height)
  ;(define-key evil-insert-state-map (kbd "C-c C-c") 'evil-esc)
  (evil-mode 1)
  (setq evil-default-state 'normal)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map (kbd "C-x C-a") 'evil-normal-state)
  (evil-set-initial-state ido-mode 'insert)
  (when (require 'surround nil t)
    (global-surround-mode 1)))

; Undo tree
(require 'undo-tree)

; Custom variables
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)
