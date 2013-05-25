;;;; jkpl's Emacs confs

;; Load paths
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/elisp")
(setq custom-theme-directory "~/.emacs.d/themes")
(when (file-accessible-directory-p "~/.emacs.d/vendor")
  (let ((default-directory "~/.emacs.d/vendor"))
    (normal-top-level-add-subdirs-to-load-path)))

;; My utils
(require 'my-utils)

(setq my-load-files
  '("~/.emacs.d/pkg-management.el"
    "~/.emacs.d/vendor/loaddefs.el"
    "~/.emacs.local"))

(setq my-keybindings-alist
  '(("C-x ," . recompile)
    ("C-x C-b" . ibuffer)
    ("C-x C-j" . join-line)
    ("C-x C-n" . other-window)
    ("C-x C-p" . other-window-back)
    ("C-x O" . other-window-back)
    ("C-x g" . compile)
    ("M-/" . hippie-expand)
    ("M-C" . region-to-clipboard)
    ("M-J" . deindent-rigidly)
    ("M-K" . indent-rigidly)
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

; Environment variables
(setenv "PAGER" "/bin/cat")

;; Xterm mouse & selection
(when (require 'mouse nil t)
  (xterm-mouse-mode t)
  (setq track-mouse nil
        mouse-sel-mode t))

;; Modes
(blink-cursor-mode 0)
(global-font-lock-mode t)
(set-input-mode nil nil t)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(ido-mode 1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Automode
(add-to-list 'auto-mode-alist '("\\.emacs.local$" . emacs-lisp-mode))

;; Settings
(setq backup-inhibited t                ; auto saving
      auto-save-default nil
      auto-save-visited-file-name t
      auto-save-interval 200
      auto-save-timeout 20)

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
      ido-default-buffer-method 'selected-window
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

(setq visible-bell nil                  ; bell
      ring-bell-function 'ignore)

(setq inhibit-splash-screen t           ; misc
      completion-cycle-threshold 0
      x-select-enable-clipboard t
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil)

(setq-default                           ; defaults
 tab-width 4
 c-basic-offset 4
 indent-tabs-mode nil
 fill-column 79
 whitespace-style '(face trailing lines-tail)
 whitespace-line-column 79)

(put 'downcase-region 'disabled nil)    ; enable disabled
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Mode settings
(defun ms-mail ()
  "Email hook function."
  (turn-on-auto-fill)
  (setq fill-column 72))

(defun ms-org ()
  "Org mode hook function."
  (setq org-hide-leading-stars t))

(defun ms-comint ()
  "Comint mode hook function."
  (setq  comint-completion-addsuffix t
         comint-completion-autolist t
         comint-input-ignoredups t
         comint-scroll-show-maximum-output t
         comint-scroll-to-bottom-on-input t
         comint-scroll-to-bottom-on-output t))

(defun ms-rst ()
  "RST hook function."
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79
        whitespace-line-column 79))

(defun ms-python ()
  "Python hook function."
  (defun pep8 (&optional buffer)
    (interactive "bPEP8 check buffer: ")
    (python-check
     (concat "pep8 "
             (buffer-file-name (get-buffer buffer)))))
  (defun pyflakes (&optional buffer)
    (interactive "bPyFlakes check buffer: ")
    (python-check
     (concat "pyflakes "
             (buffer-file-name (get-buffer buffer)))))
  (turn-on-auto-fill)
  (whitespace-mode 1)
  (local-set-key (kbd "RET") 'newline)
  (local-set-key (kbd "C-c C-q") 'jedi)
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        whitespace-line-column 79
        fill-column 79))

(defun ms-ibuffer ()
  "IBuffer hook function"
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(defun ms-eshell ()
  "EShell hook function"
  (setq eshell-prompt-function (lambda () "$ "))
  (defun eshell/clear ()
    "Clear buffer"
    (interactive)
    (let ((inhibit-read-only t)) (erase-buffer))))

;; Hooks
(add-hook 'rst-mode-hook 'ms-rst)
(add-hook 'python-mode-hook 'ms-python)
(add-hook 'org-mode-hook 'ms-org)
(add-hook 'mail-mode-hook 'ms-mail)
(add-hook 'comint-mode-hook 'ms-comint)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'ibuffer-mode-hook 'ms-ibuffer)
(add-hook 'eshell-mode-hook 'ms-eshell)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2
                  tab-width 2
                  c-basic-offset 2)))
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;; Load my stuff
(set-my-keybindings)
(set-my-aliases)
(load-my-load-files)

;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ffap-machine-p-known (quote reject)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(magit-item-highlight ((t nil)))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "grey78" :foreground "black"))) t)
 '(rst-level-3-face ((t (:background "grey71" :foreground "black"))) t)
 '(rst-level-4-face ((t (:background "grey64" :foreground "black"))) t)
 '(rst-level-5-face ((t (:background "grey57" :foreground "black"))) t)
 '(rst-level-6-face ((t (:background "grey50" :foreground "black"))) t))

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)
