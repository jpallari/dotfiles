;;;; jkpl's Emacs confs

;; Common LISP compatibility
(require 'cl)

;; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Custom envs
(setenv "PAGER" "/bin/cat")

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))
(load-file "~/.emacs.d/vendor/loaddefs.el")

;; Version specific settings
(if (>= emacs-major-version 24)
  (progn ;; Emacs 24 and newer
    nil)
  (progn ;; Emacs 23 and older
    (require 'package)
    (add-to-list 'load-path "~/.emacs.d/package/")))

;; Package management
(setq my-pkgs-alist
      '(("essential" . (fill-column-indicator expand-region undo-tree))
        ("apps" . (magit auctex w3m))
        ("modes" . (lua-mode haskell-mode markdown-mode erlang))
        ("webdev" . (js2-mode js-comint coffee-mode less-css-mode flymake-jshint flymake-coffee))))
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Custom functions
(defun install-missing-packages (pkg-list)
  "Installs all the missing packages from selected list."
  (interactive (list (completing-read "Choose a package group: " my-pkgs-alist)))
  (mapc (lambda (pkg)
          (or (package-installed-p pkg)
              (if (y-or-n-p (format "Package %s is missing. Install it? " pkg))
                  (package-install pkg))))
        (cdr (assoc pkg-list my-pkgs-alist))))

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

(defun other-window-back ()
  "Select another window backwards"
  (interactive)
  (other-window -1))

(defun raw-indent (arg)
  "Rigid indent and remain in region"
  (interactive "p")
  (save-excursion
    (let ((deactivate-mark nil))
      (indent-rigidly (region-beginning) (region-end) arg))))

(defun raw-deindent (arg)
  "Rigid deindent and remain in region"
  (interactive "p")
  (save-excursion
    (let ((deactivate-mark nil))
      (indent-rigidly (region-beginning) (region-end) (- arg)))))

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
(global-set-key (kbd "C-w") 'kr-or-bwkw)
(global-set-key (kbd "C-k") 'kr-or-kl)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x O") 'other-window-back)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-back)
(global-set-key (kbd "M-J") 'raw-deindent)
(global-set-key (kbd "M-K") 'raw-indent)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "C-x C-j") 'join-line)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "M-C") 'completion-at-point)
(global-set-key (kbd "C-t") 'completion-at-point)
(global-set-key (kbd "M-M") 'er/expand-region)
(global-set-key (kbd "M-?") 'undo-tree-redo)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [f5] 'shrink-window-horizontally)
(global-set-key [f6] 'enlarge-window)
(global-set-key [f7] 'shrink-window)
(global-set-key [f8] 'enlarge-window-horizontally)

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
      completion-cycle-threshold 4
      show-paren-delay 0.0
      scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      visible-bell nil
      ring-bell-function 'ignore)
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
(set-input-mode t nil t)
(tool-bar-mode -1)

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
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'sl 'sort-lines)
(defalias 'snf 'sort-numeric-fields)

;; Some defaults
(setq-default tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil
              fill-column 79
              whitespace-style '(face trailing)
              whitespace-line-column 79)
(setq confirm-nonexistent-file-or-buffer nil)

;; Enable disabled commands
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
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess)
(define-key ido-common-completion-map (kbd "C-z") 'keyboard-escape-quit)

;; Fill column indicator
(setq fci-rule-width 1
      fci-rule-color "#87005f"
      fci-rule-character-color "#87005f")

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

;; Automode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("dotfiles\\/emacs$" . emacs-lisp-mode))
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
(when (fboundp 'erlang-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(e\\|h\\)rl$" . erlang-mode)))

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
        jshint-configuration-path (concat (getenv "HOME") "/.jshint.json")
        js2-consistent-level-indent-inner-bracket-p t
        js2-pretty-multiline-decl-indentation-p t
        js2-basic-offset 2
        js2-strict-inconsistent-return-warning nil
        inferior-js-program-command "node")
  (flymake-mode 1)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b") 'js-send-buffer)
  (local-set-key (kbd "C-c b") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c l") 'js-load-file-and-go))
(add-hook 'js-mode-hook 'ft-js)
(add-hook 'js2-mode-hook 'ft-js2)

;; JS comint
(defun ft-js-comint ()
  (ansi-color-for-comint-mode-on)
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string
      ".*1G\.\.\..*5G" "..."
      (replace-regexp-in-string ".*1G.*3G" "> " output)))))
(setq inferior-js-mode-hook 'ft-js-comint)

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
        fill-column 79
        whitespace-line-column 79))
(add-hook 'markdown-mode-hook 'ft-markdown)

;; RST
(defun ft-rst ()
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79
        whitespace-line-column 79))
(add-hook 'rst-mode-hook 'ft-rst)

;; Python
(defun ft-python ()
  (defun pep8 (&optional buffer)
    (interactive "bPEP8 buffer: ")
    (python-check
     (concat "pep8 "
             (buffer-file-name (get-buffer buffer)))))
  (defun pyflakes (&optional buffer)
    (interactive "bPyFlakes buffer: ")
    (python-check
     (concat "pyflakes "
             (buffer-file-name (get-buffer buffer)))))
  (turn-on-auto-fill)
  (eldoc-mode 1)
  (when (fboundp 'fci-mode) (fci-mode))
  (whitespace-mode 1)
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        whitespace-line-column 79
        fill-column 79))
(add-hook 'python-mode-hook 'ft-python)

;; Haskell
(defun ft-haskell ()
  (setq tab-width 2
        haskell-indent-offset 2
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
  (when (fboundp 'fci-mode) (fci-mode))
  (whitespace-mode 1)
  (setq coffee-tab-width 2
        tab-width 2)
  (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)
  (define-key coffee-mode-map (kbd "C-m") 'newline))
(add-hook 'coffee-mode-hook 'ft-coffee)

;; C
(defun ft-c-common ()
  (setq c-basic-offset 4
        tab-width 4)
  (c-toggle-auto-state 1)
  (define-key c-mode-base-map (kbd "RET") 'indent-new-comment-line))
(add-hook 'c-mode-common-hook 'ft-c-common)

;; CSS
(defun ft-css-common ()
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'ft-css-common)

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
