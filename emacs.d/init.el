;;;; jkpl's Emacs confs

;; Load paths and files
(when (file-accessible-directory-p "~/.emacs.d/vendor")
  (let ((default-directory "~/.emacs.d/vendor"))
    (normal-top-level-add-subdirs-to-load-path)))
(setq custom-file "~/.emacs.d/custom-file.el")

(defconst my-load-files
  (list "~/.emacs.d/pkg-management.el"  ; package management
        "~/.emacs.d/vendor/loaddefs.el" ; vendor
        "~/.emacs.d/modesettings.el"    ; mode settings
        custom-file                     ; customizations file
        "~/.emacs.local"))              ; local customizations

;; Functions
(defun filtr (condp lst)
  (delq nil
        (mapcar
         (lambda (x)
           (and (funcall condp x) x))
         lst)))

;; Commands
(defun what-face (pos)
  "Displays the current face name under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

(defun other-window-back (count &optional all-frames)
  "Select another window backwards"
  (interactive "p")
  (other-window (- count) all-frames))

(defun deindent-rigidly (start end arg)
  "Same as indent-rigidly but with negative argument."
  (interactive "r\np")
  (indent-rigidly start end (- arg)))

(defun region-to-clipboard (start end)
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-ring-save start end)
    (shell-command-on-region start end "xsel -i -b"))
  (message "Region copied to clipboard"))

;; Keybindings
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "C-x C-j") 'join-line)
(global-set-key (kbd "C-x C-k") 'backward-kill-word)
(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-p") 'other-window-back)
(global-set-key (kbd "C-x O") 'other-window-back)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-C") 'region-to-clipboard)
(global-set-key (kbd "M-J") 'deindent-rigidly)
(global-set-key (kbd "M-K") 'indent-rigidly)
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
  (setq track-mouse nil
        mouse-sel-mode t))

; Custom environment variables
(setenv "PAGER" "/bin/cat")

;; Modes
(blink-cursor-mode 0)                   ; No blinking cursor
(global-font-lock-mode t)               ; Syntax coloring
(set-input-mode nil nil t)              ; No interrupt, no flow control
(column-number-mode t)                  ; Enable column number mode
(show-paren-mode t)                     ; Enable show paren mode
(transient-mark-mode t)                 ; Transient mark
(ido-mode 1)                            ; IDO
(tool-bar-mode -1)                      ; No toolbars
(menu-bar-mode -1)                      ; No menu bars

;; Settings
(setq backup-inhibited t                      ; disable backup
      auto-save-default nil                   ; disable autosave
      inhibit-splash-screen t                 ; No splash screen
      completion-cycle-threshold 0            ; No cycle threshold
      visible-bell nil                        ; No visible bell
      ring-bell-function 'ignore              ; No audible bell
      x-select-enable-clipboard t             ; X clipboard
      confirm-nonexistent-file-or-buffer nil  ; New on open
      sentence-end-double-space nil)          ; Single space sentences

(setq compilation-ask-about-save nil    ; compilation
      compilation-save-buffers-predicate '(lambda () nil))

(setq sendmail-program "/usr/bin/msmtp" ; mail
      message-send-mail-function 'message-send-mail-with-sendmail)

(setq mouse-wheel-progressive-speed nil ; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil)))

(setq ido-enable-flex-matching t        ; IDO
      ido-everywhere t
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)

(setq ibuffer-saved-filter-groups       ; ibuffer
      '(("default"
         ("Org" (mode . org-mode))
         ("Dired" (mode . dired-mode))
         ("Emacs"
          (or (name . "^\\*scratch\\*$")
              (name . "^\\*Messages\\*$")))
         ("Help"
          (or (name . "\\*Help\\*")
              (name . "\\*Apropos\\*")
              (name . "\\*info\\*")))
         ("Email"
          (or (name . "^\\*mu4e-")
              (mode . message-mode)))
         ("Calc" (name . "\\*Calc"))
         ("Elisp" (mode . emacs-lisp-mode))))
      ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'major-mode)

(setq hippie-expand-try-functions-list  ; hippie expand
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-line
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(setq tramp-default-method "sshx"       ; TRAMP
      tramp-chunkzise 500
      tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(setq c-default-style                   ; CC-mode stuff
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "k&r")))

(setq-default                           ; -- Defaults --
 tab-width 4                            ; Default tab width: 4
 c-basic-offset 4                       ; ...same for C style languages
 indent-tabs-mode nil                   ; Spaces instead of tabs
 fill-column 79                         ; Fill column: 79
 whitespace-style '(face                ; WP: use faces
                    trailing            ; WP: trailing blanks
                    lines-tail)         ; WP: long lines (tail)
 whitespace-line-column 79)

(put 'downcase-region 'disabled nil)    ; Enable downcase region
(put 'set-goal-column 'disabled nil)    ; Enable set goal column
(put 'narrow-to-region 'disabled nil)   ; Enable narrow to region

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq eshell-prompt-function (lambda () "$ "))
            (defun eshell/clear ()
              "Clear buffer"
              (interactive)
              (let ((inhibit-read-only t)) (erase-buffer)))))

;; Load custom elisp files
(mapc (lambda (filename) (load filename t t t)) my-load-files)

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)
