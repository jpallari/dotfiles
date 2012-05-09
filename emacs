;;;; jkpl's Emacs confs

;; Common Lisp compatibility
(require 'cl)

;; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Custom envs
(setenv "PAGER" "/bin/cat")

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")

;; Version specific settings
(if (>= emacs-major-version 24)
  (progn ;; Emacs 24 and newer
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'seanmod t))
  (progn ;; Emacs 23 and older
    (add-to-list 'load-path "~/.emacs.d/package/")))

;; Package management
(setq my-pkgs
  '(evil popup sws-mode auto-complete surround magit
         haskell-mode jade-mode coffee-mode markdown-mode
         python stylus-mode js2-mode undo-tree
         flymake-coffee flymake-jslint))
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Custom functions
(defun install-missing-packages ()
  "Installs all the missing packages"
  (interactive)
  (mapc
    (lambda (pkg)
      (or (package-installed-p pkg)
          (if (y-or-n-p (format "Package %s is missing. Install it? " pkg))
            (package-install pkg)))) my-pkgs))
(defun kr-or-bwkw (&optional arg region)
  "`kill-region` if the region is active, otherwise `backward-kill-word`"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Keybindings
(global-set-key (kbd "C-w") 'kr-or-bwkw)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)

;; UI
(when (not window-system)
  (menu-bar-mode -1))
(setq inhibit-splash-screen t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(eldoc-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-background 'modeline "#0000ee")
(set-face-foreground 'modeline "#ffffff")
(set-cursor-color "#ffcc22")
(set-mouse-color "#ffffff")
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
(xterm-mouse-mode)

;; Some defaults
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(turn-on-font-lock)

;; Filetype specific settings
(load "~/.emacs.d/ft")

;; Email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")

;; Notmuch
(autoload 'notmuch "~/.emacs.d/my-notmuch" "notmuch mail" t)

;; IDO mode
(when (require 'ido nil t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1))

;; Browser
(when (not (getenv "DISPLAY"))
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))
(setq w3m-use-cookies t)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; Auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB"))

;; EVIL
(when (require 'evil nil t)
  (define-key evil-normal-state-map "\C-h" 'evil-window-left)
  (define-key evil-normal-state-map "\C-j" 'evil-window-down)
  (define-key evil-normal-state-map "\C-k" 'evil-window-up)
  (define-key evil-normal-state-map "\C-l" 'evil-window-right)
  (define-key evil-normal-state-map "-" 'evil-window-decrease-height)
  (define-key evil-normal-state-map "+" 'evil-window-increase-height)
  (evil-mode 1)
  (setq evil-default-state 'normal)

  ;; emacs mode as insert mode
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map (kbd "C-x C-a") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-x C-a") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-z") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-z") 'evil-normal-state)

  ;; evil surround
  (when (require 'surround nil t)
    (global-surround-mode 1)))

;; Undo tree
(require 'undo-tree nil t)

;; Customizations file
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

;; Local settings
(let ((fname "~/.emacs.local"))
  (when (file-exists-p fname)
    (load fname)))