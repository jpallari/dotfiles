;;;; jkpl's Emacs confs

;; Load paths and files
(when (file-accessible-directory-p "~/.emacs.d/vendor")
  (let ((default-directory "~/.emacs.d/vendor"))
    (normal-top-level-add-subdirs-to-load-path)))
(setq custom-file "~/.emacs.d/custom-file.el")

;; Variables and constants
(defconst my-load-files
  (list "~/.emacs.d/pkg-management.el"  ; package management
        "~/.emacs.d/vendor/loaddefs.el" ; vendor
        "~/.emacs.d/modesettings.el"    ; mode settings
        custom-file                     ; customizations file
        "~/.emacs.local")               ; local customizations
  "List of files to load during start up.")

(defvar my-keybindings-alist
  '(("C-x C-b" . ibuffer)
    ("C-x C-j" . join-line)
    ("C-x C-n" . other-window)
    ("C-x C-p" . other-window-back)
    ("C-x O" . other-window-back)
    ("M-/" . hippie-expand)
    ("M-C" . region-to-clipboard)
    ("M-J" . deindent-rigidly)
    ("M-K" . indent-rigidly)
    ("<f5>" . shrink-window-horizontally)
    ("<f6>" . enlarge-window)
    ("<f7>" . shrink-window)
    ("<f8>" . enlarge-window-horizontally))
  "List of keybindings")

(defvar my-aliases-alist
  '((dml . delete-matching-lines)
    (dnml . delete-non-matching-lines)
    (lml . list-matching-lines)
    (qr . query-replace)
    (qrr . query-replace-regexp)
    (rr . replace-regexp)
    (sgc . set-goal-column)
    (sl . sort-lines)
    (snf . sort-numeric-fields)
    (sr . replace-string)
    (yes-or-no-p . y-or-n-p))
  "List of aliases")

(defconst ido-decorations-horizontal
  '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]"
    " [Too big]" " [Confirm]")
  "Ido decorations for horizontal listing.")

(defconst ido-decorations-vertical
  '("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]"
    " [Not readable]" " [Too big]" " [Confirm]")
  "Ido decorations for vertical listing.")

;; Functions
(defun filtr (condp lst)
  "Passes each element in LST to CONDP, and filters out the
elements where the CONDP result is nil."
  (delq nil
        (mapcar
         (lambda (x)
           (and (funcall condp x) x))
         lst)))

(defun set-my-keybindings ()
  "Sets keybindings according to `my-keybindings-alist'"
  (mapc (lambda (x)
          (global-set-key (kbd (car x)) (cdr x)))
        my-keybindings-alist))

(defun set-my-aliases ()
  "Sets aliases according to `my-aliases-alist'"
  (mapc (lambda (x)
          (defalias (car x) (cdr x)))
        my-aliases-alist))

(defun load-my-load-files ()
  "Loads files according to `my-load-files'"
  (mapc (lambda (filename)
          (load filename t t t))
        my-load-files))

(defun set-default-face-fg-bg (dark-bg dark-fg light-bg light-fg &optional frame)
  "Sets the default face's background and foreground on the
provided FRAME to either of the fg & bg pairs depending on the
current default face foreground."
  (if (string= (face-foreground 'default) "black")
      (progn (set-face-background 'default dark-bg frame)
             (set-face-foreground 'default dark-fg frame))
    (progn (set-face-background 'default light-bg frame)
           (set-face-foreground 'default light-fg frame))))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

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
  "Pastes region contents to clipboard"
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-ring-save start end)
    (shell-command-on-region start end "xsel -i -b"))
  (message "Region copied to clipboard"))

(defun flip-colors ()
  "Flips default face's background/foreground between dark and
light schemes in the current frame."
  (interactive)
  (let ((frame (selected-frame)))
    (if (display-graphic-p)
        (set-default-face-fg-bg "grey10" "grey" "white smoke" "black" frame)
      (set-default-face-fg-bg "black" "white" "white" "black" frame))))

(defun ido-vertical (&optional vertical)
  (interactive)
  (if (or vertical (not (string= (substring (car ido-decorations) 0 1) "\n")))
      (progn ;; Set vertical
        (setq ido-decorations ido-decorations-vertical)
        (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))
    (progn ;; Set horizontal
      (setq ido-decorations ido-decorations-horizontal)
      (remove-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))))

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
(ido-vertical t) ; vertical by default

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

(set-my-keybindings) ; Keybindings
(set-my-aliases)     ; Aliases
(load-my-load-files) ; Load custom elisp files

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)
