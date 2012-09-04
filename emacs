;;;; jkpl's Emacs confs

(require 'cl)                           ; CommonLisp compatibility
(setenv "PAGER" "/bin/cat")             ; custom envs

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(when (file-accessible-directory-p "~/.emacs.d/vendor")
    (let ((default-directory "~/.emacs.d/vendor"))
      (normal-top-level-add-subdirs-to-load-path)))

(when (<= emacs-major-version 23)       ; Emacs 23 and older
  (menu-bar-mode -1))

;; Custom functions and commands
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

(defun apply-gui-frame-settings (frame)
  (with-selected-frame frame
    (apply-color-theme frame)))

(defun apply-color-theme (&optional frame)
  (if (display-graphic-p)
      (progn ; GUI
        (set-frame-parameter frame 'menu-bar-lines 1)
        (set-face-background 'default "#1a1a1a" frame)
        (set-face-foreground 'default "#eeeeee" frame)
        (set-face-background 'fringe "#1a1a1a" frame)
        (set-face-background 'mode-line "#303030" frame)
        (set-face-foreground 'mode-line "#ffffff" frame)
        (set-face-background 'mode-line-inactive "#121212" frame)
        (set-face-foreground 'mode-line-inactive "#767676" frame))
    (progn ; terminal
      (set-frame-parameter frame 'menu-bar-lines 0)
      (set-face-background 'default "#000000" frame)
      (set-face-foreground 'default "#dadada" frame))))

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
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'lml 'list-matching-lines)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(defalias 'sgc 'set-goal-column)
(defalias 'sl 'sort-lines)
(defalias 'snf 'sort-numeric-fields)
(defalias 'sr 'replace-string)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Xterm mouse & selection
(when (require 'mouse nil t)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; Settings
(blink-cursor-mode 0)                    ; No blinking cursor
(global-font-lock-mode t)                ; Syntax coloring
(set-input-mode nil nil t)               ; No interrupt, no flow control
(show-paren-mode (column-number-mode t)) ; Enable show-paren-mode
(tool-bar-mode -1)                       ; No toolbar
(transient-mark-mode t)                  ; Transient mark
(apply-color-theme)                      ; Color theme

(setq backup-inhibited t                      ; disable backup
      auto-save-default nil                   ; disable autosave
      inhibit-splash-screen t                 ; No splash screen
      completion-cycle-threshold 3            ; Cycle treshold: 3
      visible-bell nil                        ; No visible bell
      ring-bell-function 'ignore              ; No audible bell
      custom-file "~/.emacs.d/custom-file.el" ; Custom file
      x-select-enable-clipboard t             ; X clipboard
      confirm-nonexistent-file-or-buffer nil  ; New on open
      mouse-wheel-progressive-speed nil       ; No progressive mouse scroll
      mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil))
      default-frame-alist '((vertical-scroll-bars . right)
                            (menu-bar-lines . 0)))

(setq-default
 tab-width 4                            ; Default tab width: 4
 c-basic-offset 4                       ; ...same for C style languages
 indent-tabs-mode nil                   ; Spaces instead of tabs
 fill-column 79                         ; Fill column: 79
 whitespace-style '(face trailing)      ; Trailing whitespace
 whitespace-line-column 79)

(put 'downcase-region 'disabled nil)    ; Enable downcase region
(put 'set-goal-column 'disabled nil)    ; Enable set goal column
(put 'narrow-to-region 'disabled nil)   ; Enable narrow to region

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

;; Hooks
(add-hook 'after-make-frame-functions 'apply-gui-frame-settings)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load custom elisp files
(load "~/.emacs.d/pkg-management.el" t t t)  ; package management
(load "~/.emacs.d/vendor/loaddefs.el" t t t) ; vendor
(load "~/.emacs.d/modesettings.el" t t t)    ; mode settings
(load custom-file t t t)                     ; customizations file
(load "~/.emacs.local" t t t)                ; local customizations

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8)
