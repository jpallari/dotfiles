;;; Load paths

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/elisp")
(setq custom-theme-directory "~/.emacs.d/themes")
(when (file-accessible-directory-p "~/.emacs.d/vendor")
  (let ((default-directory "~/.emacs.d/vendor"))
    (normal-top-level-add-subdirs-to-load-path)))

;;; My utils
(require 'my-utils)

(setq my-load-files
  '("~/.emacs.d/pkg-management.el"
    "~/.emacs.d/vendor/loaddefs.el"
    "~/.emacs.local"))

(setq my-keybindings-alist
  '(("C-x ," . recompile)
    ("C-x C-b" . ibuffer)
    ("C-x O" . other-window-back)
    ("C-x g" . compile)
    ("M-/" . hippie-expand)
    ("M-C" . region-to-clipboard)
    ("M-V" . paste-from-clipboard)
    ("<f5>" . shrink-window-horizontally)
    ("<f6>" . enlarge-window)
    ("<f7>" . shrink-window)
    ("<f8>" . enlarge-window-horizontally)))

(setq my-aliases-alist
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
    (yes-or-no-p . y-or-n-p)))

;;; Environment variables
(setenv "PAGER" "/bin/cat")

;;; Mouse
(when (require 'mouse nil t)
  (xterm-mouse-mode t)
  (setq track-mouse nil
        mouse-sel-mode t))
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil)))

;;; Modes
(global-font-lock-mode t)
(set-input-mode nil nil t)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(ido-mode 1)
(winner-mode 1)
(delete-selection-mode 1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; Automode
(add-to-list 'auto-mode-alist '("\\.emacs.local$" . emacs-lisp-mode))

;;; Settings

;; Auto saving
(setq backup-inhibited t
      auto-save-default nil
      auto-save-visited-file-name t
      auto-save-interval 0
      auto-save-timeout 4)

;; Compilation
(setq compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil))

;; Email
(setq sendmail-program "/usr/bin/msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail)

(defun ms-mail ()
  "Email hook function"
  (setq fill-column 72))

(add-hook 'mail-mode-hook #'ms-mail)

;; IDO
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-use-filename-at-point 'guess
      ido-auto-merge-delay-time 9999)
(define-key ido-file-completion-map (kbd "C-c C-s")
  (lambda ()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))
(ido-vertical t) ; vertical by default

;; IBuffer
(setq ibuffer-saved-filter-groups
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

(defun ms-ibuffer ()
  "IBuffer hook function"
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook #'ms-ibuffer)

;; Hippie expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-line
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; TRAMP
(setq tramp-default-method "sshx"
      tramp-chunkzise 500
      tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; Bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Killing Emacs
(setq confirm-kill-emacs
      'kill-emacs-y-or-n-p)

;; Indentation
(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 80
              whitespace-style '(face trailing lines-tail)
              cursor-type 'bar)

;; Enable disabled features
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Comint
(setq-default comint-completion-addsuffix t
              comint-completion-autolist t
              comint-input-ignoredups t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output t)

;; Python
(setq-default python-indent-offset 4)

(defun ms-python ()
  "Python hook function"
  (defun pep8 (&optional buffer)
    (interactive "bPEP8 check buffer: ")
    (python-check
     (concat "pep8 "
             (buffer-file-name (get-buffer buffer)))))
  (defun pyflakes (&optional buffer)
    (interactive "bPyFlakes check buffer: ")
    (python-check
     (concat "pyflakes "
             (buffer-file-name (get-buffer buffer))))))

(add-hook 'python-mode-hook #'ms-python)

;; JavaScript
(setq-default js-indent-level 2)

;; ORG
(setq-default org-hide-leading-stars t)

;; CSS
(setq-default css-indent-offset 2)

;; EShell
(defun ms-eshell ()
  "EShell hook function"
  (setq eshell-prompt-function (lambda () "$ "))
  (defun eshell/clear ()
    "Clear buffer"
    (interactive)
    (let ((inhibit-read-only t)) (erase-buffer))))

(add-hook 'eshell-mode-hook #'ms-eshell)

;; CC mode
(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "k&r"))
              c-basic-offset 4)

;; Misc
(setq inhibit-splash-screen t
      completion-cycle-threshold 0
      x-select-enable-clipboard t
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil
      ffap-machine-p-known 'reject)

;;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; Menu bar only on GUI mode
(defun my-menu-bar-for-frame (frame)
  (if (memq (window-system frame) '(ns x))
      (set-frame-parameter frame 'menu-bar-lines 1)
    (set-frame-parameter frame 'menu-bar-lines 0)))
(add-hook 'after-make-frame-functions 'my-menu-bar-for-frame)
(menu-bar-mode (if (display-graphic-p) 1 -1))

;;; Custom key-to-key mappings (for terminals)
(define-key input-decode-map "\e[1;5A" (kbd "C-<up>"))
(define-key input-decode-map "\e[1;5B" (kbd "C-<down>"))
(define-key input-decode-map "\e[1;5C" (kbd "C-<right>"))
(define-key input-decode-map "\e[1;5D" (kbd "C-<left>"))

(define-key input-decode-map "\e[1;3A" (kbd "M-<up>"))
(define-key input-decode-map "\e[1;3B" (kbd "M-<down>"))
(define-key input-decode-map "\e[1;3C" (kbd "M-<right>"))
(define-key input-decode-map "\e[1;3D" (kbd "M-<left>"))

(define-key input-decode-map "\e[1;2A" (kbd "S-<up>"))
(define-key input-decode-map "\e[1;2B" (kbd "S-<down>"))
(define-key input-decode-map "\e[1;2C" (kbd "S-<right>"))
(define-key input-decode-map "\e[1;2D" (kbd "S-<left>"))

(define-key input-decode-map "\e[1;7A" (kbd "C-M-<up>"))
(define-key input-decode-map "\e[1;7B" (kbd "C-M-<down>"))
(define-key input-decode-map "\e[1;7C" (kbd "C-M-<right>"))
(define-key input-decode-map "\e[1;7D" (kbd "C-M-<left>"))

;;; OS X specific customizations
(when (eq system-type 'darwin)
  ;; Custom bindings
  (global-unset-key (kbd "s-q"))
  (global-unset-key (kbd "s-w"))
  (global-set-key (kbd "s-<right>") 'move-end-of-line)
  (global-set-key (kbd "s-<left>") 'move-beginning-of-line)
  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)

  ;; Right option as alt gr
  (setq mac-right-option-modifier nil))

;;; Load my stuff
(set-my-keybindings)
(set-my-aliases)
(load-my-load-files)

;;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elm-mode ox-pandoc htmlize yaml-mode win-switch utop use-package smartparens scala-mode2 scala-mode sbt-mode rust-mode paredit markdown-mode magit less-css-mode js2-mode js-comint iedit groovy-mode gradle-mode go-mode ghc fuzzy flymake-python-pyflakes expand-region exec-path-from-shell erlang auctex ag ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(magit-item-highlight ((t nil)) t))

;;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)
