;;; Load paths

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor")
(setq custom-theme-directory "~/.emacs.d/themes")
(when (file-accessible-directory-p "~/.emacs.d/vendor")
  (let ((default-directory "~/.emacs.d/vendor"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Variables
(defvar my-clipboard-copy "xsel -b -i" "Shell clipboard copy command")
(defvar my-clipboard-paste "xsel -b -o" "Shell clipboard paste command")

;;; Functions
(defun filter-list (condp lst)
  "Passes each element in LST to CONDP, and filters out the
elements where the CONDP result is nil."
  (delq nil
        (mapcar (lambda (x)
                  (and (funcall condp x) x))
                lst)))

(defun string-ends-with (str ending)
  "Return non-nil if STR ends with ENDING."
  (string= (substring str (- 0 (length ending))) ending))

(defun subdirectories-of-directory (directory &optional full match nosort)
  "Gets a list of all the subdirectories in DIRECTORY. The
parameters FULL, MATCH, and NOSORT work the same as in
`directory-files-and-attributes`."
  (filter-list (lambda (file)
                  (and (eq (car (cdr file)) t)                 ; is directory
                       (not (string-ends-with (car file) ".")) ; isn't "."
                       (car file)))                            ; is not empty
                (directory-files-and-attributes directory full match nosort)))

(defun update-directory-loaddefs (directory)
  "Scans the autoloads from all the subdirectories of DIRECTORY,
and writes them to the loaddefs.el file of DIRECTORY"
  (let* ((generated-autoload-file (concat directory "/loaddefs.el"))
         (dirs-with-info (subdirectories-of-directory directory t))
         (dirs (mapcar 'car dirs-with-info)))
    (apply 'update-directory-autoloads dirs)))

;;; Commands
(defun what-face (pos)
  "Displays the current face name under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

(defun region-to-clipboard (start end)
  "Copies region contents to clipboard"
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-ring-save start end)
    (shell-command-on-region start end my-clipboard-copy))
  (message "Region copied to clipboard"))

(defun paste-from-clipboard ()
  "Pastes clipboard contents to buffer"
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string my-clipboard-paste))))

(defun update-vendor-loaddefs ()
  "Update loaddefs.el file for vendor directory."
  (interactive)
  (update-directory-loaddefs "~/.emacs.d/vendor"))

(defun toggle-delete-trailing-whitespace ()
  "Toggles trailing whitespace deletion during save."
  (interactive)
  (if (member 'delete-trailing-whitespace before-save-hook)
      (remove-hook 'before-save-hook 'delete-trailing-whitespace)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(defun org-file ()
  "Open a file from the Org directory."
  (interactive)
  (let ((default-directory (concat org-directory "/")))
    (call-interactively 'ido-find-file)))

;;; Repos
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://stable.melpa.org/packages/")))

;;; Bindings
(global-set-key (kbd "C-x ,") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") 'other-window-back)
(global-set-key (kbd "C-x g") 'compile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-C") 'region-to-clipboard)
(global-set-key (kbd "M-V") 'paste-from-clipboard)
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window-horizontally)

;;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

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
(electric-pair-mode 1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Automode
(add-to-list 'auto-mode-alist '("\\.emacs.local$" . emacs-lisp-mode))

;;; Auto saving
(setq backup-inhibited t
      auto-save-default nil
      auto-save-visited-file-name t
      auto-save-interval 0
      auto-save-timeout 4)

;;; Compilation
(setq compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil))

;;; Email
(setq sendmail-program "/usr/bin/msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail)

(defun ms-mail ()
  "Email hook function"
  (setq fill-column 72))

(add-hook 'mail-mode-hook #'ms-mail)

;;; IDO
(defconst ido-decorations-horizontal
  '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]"
    " [Too big]" " [Confirm]")
  "Ido decorations for horizontal listing.")

(defconst ido-decorations-vertical
  '("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]"
    " [Not readable]" " [Too big]" " [Confirm]")
  "Ido decorations for vertical listing.")

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

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun ido-vertical (&optional arg)
  "Switches between vertical and horizontal style of listing in
IDO. Always switches to vertical style if ARG is non-nil."
  (interactive)
  (if (or arg (not (string= (substring (car ido-decorations) 0 1) "\n")))
      (progn ;; Set vertical
        (setq ido-decorations ido-decorations-vertical)
        (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))
    (progn ;; Set horizontal
      (setq ido-decorations ido-decorations-horizontal)
      (remove-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))))

(ido-vertical t) ; vertical by default

;;; IBuffer
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

;;; Hippie expand
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

;;; TRAMP
(setq tramp-default-method "sshx"
      tramp-chunkzise 500
      tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;;; Bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;;; Killing Emacs
(defun kill-emacs-y-or-n-p (prompt)
  "The prompt used when killing Emacs.
Ask user a \"y or n\" question only when server has been started."
  (or (not (fboundp 'server-running-p))
      (not (server-running-p))
      (y-or-n-p (concat "Server is running. " prompt))))

(setq confirm-kill-emacs
      'kill-emacs-y-or-n-p)

;;; Indentation
(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 80
              whitespace-style '(face trailing lines-tail))

;;; Enable disabled features
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Comint
(setq-default comint-completion-addsuffix t
              comint-completion-autolist t
              comint-input-ignoredups t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output t)

;;; Python
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

;;; JavaScript
(setq-default js-indent-level 2)

;;; ORG
(setq-default org-hide-leading-stars t)

;;; CSS
(setq-default css-indent-offset 2)

;;; EShell
(defun ms-eshell ()
  "EShell hook function"
  (setq eshell-prompt-function (lambda () "$ "))
  (defun eshell/clear ()
    "Clear buffer"
    (interactive)
    (let ((inhibit-read-only t)) (erase-buffer))))

(add-hook 'eshell-mode-hook #'ms-eshell)

;;; CC mode
(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "k&r"))
              c-basic-offset 4)

;;; Misc settings
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
  (set-frame-parameter frame 'menu-bar-lines
                       (if (memq (window-system frame) '(ns x)) 1 0)))
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

;;; Assuming all terminals have dark background
(unless (display-graphic-p)
  (setq frame-background-mode 'dark))

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

;;; use-package
(if (require 'use-package nil 'noerror)
    (progn
      (require 'diminish nil 'noerror)
      (require 'bind-key nil 'noerror))
  (progn
    (message "use-package not found")
    ;; If use-package is not installed, just create a dummy macro to replace it.
    (defmacro use-package (&rest args) nil)))

(use-package expand-region
  :bind ("M-M" . er/expand-region))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package win-switch
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-idle-time 1)
  (win-switch-delete-key "i" 'up)
  (win-switch-delete-key "I" 'enlarge-vertically)
  (win-switch-delete-key "o" 'next-window)
  (win-switch-add-key "n" 'next-window)
  (win-switch-add-key "h" 'left)
  (win-switch-add-key "j" 'down)
  (win-switch-add-key "k" 'up)
  (win-switch-add-key "H" 'shrink-horizontally)
  (win-switch-add-key "J" 'shrink-vertically)
  (win-switch-add-key "K" 'enlarge-vertically)
  (win-switch-add-key "i" 'split-horizontally))

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

(use-package iedit
  :bind ("M-N" . iedit-mode))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start nil
        ac-auto-show-menu nil
        ac-ignore-case t)
  (ac-set-trigger-key "TAB"))

(use-package magit
  :defer t
  :init
  (defalias 'git-st 'magit-status))

(use-package markdown-mode
  :mode
  "\\.markdown\\'" "\\.md\\'" "\\.text\\'")

(use-package clojure-mode
  :mode "\\.clj\\'"
  :init
  (use-package cider
    :commands cider-minor-mode
    :init
    (add-hook 'cider-mode-hook #'eldoc-mode))
  :config
  (add-hook 'clojure-mode-hook #'eldoc-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  :config
  (add-hook 'go-mode-hook #'subword-mode))

(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c ." . haskell-mode-format-imports)
              ("C-j"   . haskell-newline-and-indent)
              ("C-m"   . newline))
  :config
  (setq-default haskell-indent-offset 4
                haskell-indentation-layout-offset 4
                haskell-indentation-left-offset 4
                haskell-indentation-ifte-offset 4)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indendation)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook #'subword-mode))

(use-package AucTex
  :defer t
  :config
  (setq-default TeX-auto-save t
                TeX-PDF-mode t
                TeX-parse-self t
                LaTeX-verbatim-environments '("Verbatim" "lstlisting"))
  (add-to-list 'TeX-command-list
               '("Biber" "biber %s.bcf" TeX-run-BibTeX nil t)))

(use-package js2-mode
  :mode "\\.js\\'"
  :bind (:map js2-mode-map
              ("C-x C-e" . js-send-last-sexp)
              ("C-M-x"   . js-send-last-sexp-and-go)
              ("C-c b"   . js-send-buffer)
              ("C-c C-b" . js-send-buffer-and-go)
              ("C-c g"   . js-send-region)
              ("C-c C-g" . js-send-region-and-go)
              ("C-c l"   . js-load-file-and-go))
  :config
  (setq-default js2-basic-offset 2
                js2-strict-inconsistent-return-warning nil
                inferior-js-program-command "node")
  (setq-default
   inferior-js-mode-hook
   (lambda ()
     (ansi-color-for-comint-mode-filter)
     (subword-mode)
     (add-to-list
      'comint-preoutput-filter-functions
      (lambda (output)
        (replace-regexp-in-string "\e\\[[0-9]+[GKJ]" "" output)))
     (setq comint-process-echoes t)))
  (add-hook 'js2-mode-hook #'subword-mode))

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'subword-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package projectile
  :config
  (if (fboundp 'neotree-projectile-action)
      (setq projectile-switch-project-action 'neotree-projectile-action)))

(use-package neotree
  :bind ("<f9>" . neotree-toggle)
  :config
  (setq neo-autorefresh nil
        neo-force-change-root t
        neo-show-hidden-files t
        neo-toggle-window-keep-p t
        neo-vc-integration '(face char)))

(use-package ace-window
  :bind ("M-P" . ace-window)
  :config
  (setq aw-background nil))

;;; Load my stuff
(mapc (lambda (filename) (load filename t t t))
      '("~/.emacs.d/vendor/loaddefs.el"
        "~/.emacs.local"))

;;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window neotree projectile terraform-mode smex elm-mode ox-pandoc htmlize yaml-mode win-switch utop use-package scala-mode2 scala-mode sbt-mode rust-mode paredit markdown-mode magit less-css-mode js2-mode js-comint iedit groovy-mode gradle-mode go-mode ghc fuzzy expand-region exec-path-from-shell auctex ag ac-cider clojure-mode cider go-rename go-eldoc go-autocomplete auto-complete)))
 '(projectile-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type graphic)) (:background "#282a36" :foreground "#eeeefa" :height 90))))
 '(aw-leading-char-face ((t (:background "red" :foreground "white" :height 2.0))))
 '(fringe ((t nil)))
 '(magit-item-highlight ((t nil)) t)
 '(mode-line ((((type graphic)) (:background "#484a76" :foreground "#ffffff" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((((type graphic)) (:foreground "green"))))
 '(mode-line-inactive ((((type graphic)) (:inherit mode-line :background "#383a76" :foreground "#aaaaaa" :box (:line-width -1 :color "#484a76") :weight light))))
 '(region ((t (:background "#aaaaff" :foreground "#333333"))))
 '(vertical-border ((((type graphic)) (:foreground "#484a56")))))

;;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)
