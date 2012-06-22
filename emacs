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
    nil)
  (progn ;; Emacs 23 and older
    (add-to-list 'load-path "~/.emacs.d/package/")))

;; Package management
(setq my-pkgs
  '(evil sws-mode evil surround magit lua-mode python iy-go-to-char
         haskell-mode jade-mode coffee-mode markdown-mode expand-region
         stylus-mode js2-mode undo-tree auctex less-css-mode
         flymake-coffee flymake-jslint))
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Custom functions
(defun install-missing-packages ()
  "Installs all the missing packages"
  (interactive)
  (mapc (lambda (pkg)
          (or (package-installed-p pkg)
              (if (y-or-n-p (format "Package %s is missing. Install it? " pkg))
                  (package-install pkg))))
        my-pkgs))

(defun kr-or-bwkw (&optional arg region)
  "`kill-region` if the region is active, otherwise `backward-kill-word`"
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun kr-or-kl (&optional arg region)
  "`kill-region` if the region is active, otherwise `kill-line`"
  (interactive (list current-prefix-arg (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(defun what-face (pos)
  "Displays the current face name under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun jump-to-last-mark ()
  "Jumps to last set mark."
  (interactive) (set-mark-command 1))

(defun keybind-ret ()
  "Binds RET to either indent-new-comment-line or newline depending on the existing binding."
  (interactive)
  (let ((rb (key-binding (kbd "RET")))
        (set-ret (lambda (f)
                   (global-set-key (kbd "RET") f)
                   (message (format "RET binded to %s" f)))))
    (cond ((eq rb 'newline) (funcall set-ret 'indent-new-comment-line))
          ((eq rb 'indent-new-comment-line) (funcall set-ret 'newline))
          ((eq rb 'newline-and-indent) (funcall set-ret 'indent-new-comment-line)))))

(defun buffer-list-switch ()
  "Open buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0)))

(defun apply-settings-terminal (&optional frame)
  "Applies terminal specific settings."
  (set-frame-parameter frame 'menu-bar-lines 0)
  (set-face-background 'mode-line "#0000ee" frame)
  (set-face-foreground 'mode-line "#ffffff" frame)
  (set-face-background 'mode-line-inactive "#00005f" frame)
  (set-face-foreground 'mode-line-inactive "#767676" frame)
  (set-face-background 'default "#000000" frame)
  (set-face-foreground 'default "#dadada" frame))

(defun apply-settings-gui (&optional frame)
  "Applies settings used in GUI environment."
  (set-frame-parameter frame 'menu-bar-lines 1)
  (set-face-background 'mode-line "#2e3436" frame)
  (set-face-foreground 'mode-line "#eeeeee" frame)
  (set-face-background 'mode-line-inactive "#111111" frame)
  (set-face-foreground 'mode-line-inactive "#cccccc" frame)
  (set-face-background 'default "#1a1a1a" frame)
  (set-face-foreground 'default "#eeeeee" frame)
  (set-face-background 'fringe "#1a1a1a" frame))

(defun apply-settings-frame (frame)
  "Applies GUI or terminal settings for frame depending on which
one the frame is runned on."
  (with-selected-frame frame
    (if (not (display-graphic-p))
        (apply-settings-terminal frame)
        (apply-settings-gui frame))))

;; Keybindings
(global-set-key (kbd "M-`") 'jump-to-last-mark)
(global-set-key (kbd "C-w") 'kr-or-bwkw)
(global-set-key (kbd "C-k") 'kr-or-kl)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-z") 'keyboard-escape-quit)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "RET") 'indent-new-comment-line)
(global-set-key (kbd "C-x C-j") 'join-line)
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x C-b") 'buffer-list-switch)
(global-set-key (kbd "M-C") 'completion-at-point)
(global-set-key [f5] 'shrink-window-horizontally)
(global-set-key [f6] 'enlarge-window)
(global-set-key [f7] 'shrink-window)
(global-set-key [f8] 'enlarge-window-horizontally)
(global-set-key [f9] 'keybind-ret)
(global-set-key [f11] 'previous-buffer)
(global-set-key [f12] 'next-buffer)
(if (fboundp 'iy-go-to-char)
    (global-set-key (kbd "M-L") 'iy-go-to-char))

;; Theme per frame
(add-hook 'after-make-frame-functions 'apply-settings-frame)
(if (window-system)
    (apply-settings-gui)
    (apply-settings-terminal))

;; Xterm mouse & selection
(when (require 'mouse nil t)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))
(setq x-select-enable-clipboard t)

;; Other UI settings
(blink-cursor-mode 0)
(show-paren-mode (column-number-mode t))
(global-font-lock-mode t)
(transient-mark-mode t)
(setq inhibit-splash-screen t
      completion-cycle-threshold 10
      show-paren-delay 0.0)
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
(set-input-mode t nil t)

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'wins 'shrink-window)
(defalias 'wine 'enlarge-window)
(defalias 'winsh 'shrink-window-horizontally)
(defalias 'wineh 'enlarge-window-horizontally)
(defalias 'sgc 'set-goal-column)
(defalias 'git-st 'magit-status)
(defalias 'bls 'buffer-list-switch)
(defalias 'sr 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)

;; Some defaults
(setq-default tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil
              fill-column 79)
(setq confirm-nonexistent-file-or-buffer nil)

;; Enable disabled commands
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Notmuch
(autoload 'notmuch "~/.emacs.d/my-notmuch" "notmuch mail" t)

;; EVIL
(autoload 'evil-mode "~/.emacs.d/my-evil" "EVIL mode" t)

;; IDO mode
(when (require 'ido nil t)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess)
  (ido-mode 1)
  (define-key ido-common-completion-map (kbd "C-z") 'keyboard-escape-quit))

;; Expand region
(when (require 'expand-region nil t)
  (global-set-key (kbd "M-M") 'er/expand-region))

;; TRAMP
;; (if window-system
;;     (progn ;; For some strange reason, TRAMP doesn't work well with terminal.
;;       (setq tramp-default-method "scp"
;;             tramp-chunkzise 500
;;             tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))
;;   (setq tramp-mode nil))
(setq tramp-mode nil)

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

;; Undo tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode 1)
  (global-set-key (kbd "M-?") 'undo-tree-redo))

;; Automode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("dotfiles\\/emacs$" . emacs-lisp-mode))
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; Emacs LISP
(defun ft-elisp ()
  (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'ft-elisp)

;; JavaScript
(defun ft-js ()
  (setq js-indent-level 2
        tab-width 2
        c-basic-offset 2))
(defun ft-js2 ()
  (setq tab-width 2
        c-basic-offset 2
        js2-consistent-level-indent-inner-bracket-p t
        js2-pretty-multiline-decl-indentation-p t
        js2-basic-offset 2))
(add-hook 'js-mode-hook 'ft-js)
(add-hook 'js2-mode-hook 'ft-js2)

;; Magit
(defun ft-magit ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'magit-log-edit-mode-hook 'ft-magit)

;; Markdown
(defun ft-markdown ()
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79))
(add-hook 'markdown-mode-hook 'ft-markdown)

;; Python
(defun ft-python ()
  (turn-on-auto-fill)
  (eldoc-mode 1)
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        show-trailing-whitespace t
        fill-column 79))
(add-hook 'python-mode-hook 'ft-python)

;; Haskell
(defun ft-haskell ()
  (setq tab-width 2
        c-basic-offset 2)
  (define-key haskell-mode-map (kbd "C-c =") 'haskell-indent-insert-equal)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-c .") 'haskell-mode-format-imports)
  (turn-on-haskell-indent))
(add-hook 'haskell-mode-hook 'ft-haskell)

;; CoffeeScript
(defun ft-coffee ()
  (make-local-variable 'tab-width)
  (setenv "NODE_NO_READLINE" "1")
  (setq coffee-tab-width 2
        tab-width 2
        show-trailing-whitespace t)
  (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer))
(add-hook 'coffee-mode-hook 'ft-coffee)

;; C
(defun ft-c-common ()
  (setq c-basic-offset 4
        tab-width 4)
  (c-toggle-auto-state 1)
  (define-key c-mode-base-map (kbd "RET") 'indent-new-comment-line))
(add-hook 'c-mode-common-hook 'ft-c-common)

;; Email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(defun ft-mail ()
  (turn-on-auto-fill)
  (setq fill-column 72))
(add-hook 'mail-mode-hook 'ft-mail)

;; Customizations file
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

;; Local settings
(let ((fname "~/.emacs.local"))
  (when (file-exists-p fname)
    (load fname)))

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
