;;;; jkpl's Emacs confs

;; Common LISP compatibility
(require 'cl)

;; Disable backup and autosave
(setq backup-inhibited t
      auto-save-default nil)

;; Custom envs
(setenv "PAGER" "/bin/cat")

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(when (file-accessible-directory-p "~/.emacs.d/vendor")
    (let ((default-directory "~/.emacs.d/vendor"))
      (normal-top-level-add-subdirs-to-load-path))
    (load "~/.emacs.d/vendor/loaddefs.el" t t t))

;; Version specific settings
(when (<= emacs-major-version 23) ;; Emacs 23 and older
  (menu-bar-mode -1))

;; Custom functions
(defun what-face (pos)
  "Displays the current face name under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun other-window-back ()
  "Select another window backwards"
  (interactive)
  (other-window -1))

(defun deindent-rigidly (start end arg)
  "Same as indent-rigidly but with negative argument."
  (interactive "r\np")
  (indent-rigidly start end (- arg)))

(defun auto-fill ()
  "Toggles auto fill mode and fill column indicator, if it exists."
  (interactive)
  (let ((val (if (symbol-value 'auto-fill-function) -1 1)))
    (auto-fill-mode val)
    (if (fboundp 'fci-mode) (fci-mode val))))

(defun apply-settings-terminal (&optional frame)
  "Applies terminal specific settings."
  (set-frame-parameter frame 'menu-bar-lines 0)
  (set-face-background 'default "#000000" frame)
  (set-face-foreground 'default "#dadada" frame))

(defun apply-settings-gui (&optional frame)
  "Applies settings used in GUI environment."
  (set-frame-parameter frame 'menu-bar-lines 1)
  (set-face-background 'default "#1a1a1a" frame)
  (set-face-foreground 'default "#eeeeee" frame)
  (set-face-background 'fringe "#1a1a1a" frame))

(defun apply-settings-frame (frame)
  "Apply GUI or terminal specific settings for frame."
  (with-selected-frame frame
    (if (not (display-graphic-p))
        (apply-settings-terminal frame)
        (apply-settings-gui frame))))

;; Theme
(set-face-background 'mode-line "#303030")
(set-face-foreground 'mode-line "#ffffff")
(set-face-background 'mode-line-inactive "#121212")
(set-face-foreground 'mode-line-inactive "#767676")

;; Frame settings
(add-hook 'after-make-frame-functions 'apply-settings-frame)
(if (window-system)
    (apply-settings-gui)
    (apply-settings-terminal))

;; Keybindings
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-t") 'completion-at-point)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "C-x C-j") 'join-line)
(global-set-key (kbd "C-x C-k") 'backward-kill-word)
(global-set-key (kbd "C-x O") 'other-window-back)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-J") 'deindent-rigidly)
(global-set-key (kbd "M-K") 'indent-rigidly)
(global-set-key (kbd "M-O") 'other-window-back)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key [f5] 'shrink-window-horizontally)
(global-set-key [f6] 'enlarge-window)
(global-set-key [f7] 'shrink-window)
(global-set-key [f8] 'enlarge-window-horizontally)

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sgc 'set-goal-column)
(defalias 'git-st 'magit-status)
(defalias 'sr 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'sl 'sort-lines)
(defalias 'snf 'sort-numeric-fields)

;; Xterm mouse & selection
(when (require 'mouse nil t)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil))
      mouse-wheel-progressive-speed nil)
(setq x-select-enable-clipboard t)

;; UI settings
(blink-cursor-mode 0)
(show-paren-mode (column-number-mode t))
(global-font-lock-mode t)
(transient-mark-mode t)
(setq inhibit-splash-screen t
      completion-cycle-threshold 4
      visible-bell nil
      ring-bell-function 'ignore)
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
(set-input-mode t nil t)
(tool-bar-mode -1)

;; Some defaults
(setq-default tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil
              fill-column 79
              whitespace-style '(face trailing)
              whitespace-line-column 79)
(setq confirm-nonexistent-file-or-buffer nil)

;; Enable disabled features
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Notmuch
(autoload 'notmuch "~/.emacs.d/my-notmuch" "notmuch mail" t)

;; EShell
(setq eshell-prompt-function (lambda () "$ "))
(defun eshell/clear ()
  "Clear buffer"
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

;; IDO mode
(when (fboundp 'ido-mode)
  (ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess))

;; Hippie expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; TRAMP
(setq tramp-default-method "sshx"
      tramp-chunkzise 500
      tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; Browser
(when (not (getenv "DISPLAY"))
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))
(setq w3m-use-cookies t
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package management
(load "~/.emacs.d/pkg-management.el" t t t)

;; Automode
(add-to-list 'auto-mode-alist '("dotfiles\\/emacs$" . emacs-lisp-mode))
(when (fboundp 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode)))
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
(when (fboundp 'erlang-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(e\\|h\\)rl$" . erlang-mode)))

;; Mode settings
(load "~/.emacs.d/modesettings.el" t t t)

;; Customizations file
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file t t t)

;; Local settings
(load "~/.emacs.local" t t t)

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
