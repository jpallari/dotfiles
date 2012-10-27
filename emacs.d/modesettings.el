;;;; modesettings.el -- settings for different kinds of modes

;; Autoloads
(autoload 'notmuch "~/.emacs.d/my-notmuch" "notmuch mail" t)

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
(when (fboundp 'clojure-mode)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

;; Emacs LISP
(defun ms-elisp ()
  (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'ms-elisp)

;; LISP
(defun ms-lisp ()
  (when (not (featurep 'slime))
    (require 'slime)))
(add-hook 'lisp-mode-hook 'ms-lisp)

;; SLIME REPL
(defun ms-slime-repl ()
  (local-set-key (kbd "C-t") 'slime-complete-symbol))
(add-hook 'slime-repl-mode-hook 'ms-slime-repl)

;; nREPL
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; JavaScript
(defun ms-js ()
  (setq js-indent-level 2
        tab-width 2
        c-basic-offset 2))
(defun ms-js2 ()
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
(add-hook 'js-mode-hook 'ms-js)
(add-hook 'js2-mode-hook 'ms-js2)

;; JS comint
(defun ms-js-comint ()
  (ansi-color-for-comint-mode-on)
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string
      ".*1G\.\.\..*5G" "..."
      (replace-regexp-in-string ".*1G.*3G" "> " output)))))
(setq inferior-js-mode-hook 'ms-js-comint)

;; Magit
(defun ms-magit ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'magit-log-edit-mode-hook 'ms-magit)

;; Markdown
(defun ms-markdown ()
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79
        whitespace-line-column 79))
(add-hook 'markdown-mode-hook 'ms-markdown)

;; RST
(defun ms-rst ()
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79
        whitespace-line-column 79))
(add-hook 'rst-mode-hook 'ms-rst)

;; Python
(defun ms-python ()
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
  (whitespace-mode 1)
  (local-set-key (kbd "RET") 'newline)
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        whitespace-line-column 79
        fill-column 79))
(add-hook 'python-mode-hook 'ms-python)

;; Haskell
(defun ms-haskell ()
  (setq tab-width 2
        haskell-indent-offset 2
        c-basic-offset 2)
  (define-key haskell-mode-map (kbd "C-c =") 'haskell-indent-insert-equal)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-c .") 'haskell-mode-format-imports)
  (turn-on-haskell-indent))
(add-hook 'haskell-mode-hook 'ms-haskell)

;; CoffeeScript
(defun ms-coffee ()
  (make-local-variable 'tab-width)
  (setenv "NODE_NO_READLINE" "1")
  (whitespace-mode 1)
  (setq coffee-tab-width 2
        tab-width 2)
  (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)
  (define-key coffee-mode-map (kbd "C-m") 'newline))
(add-hook 'coffee-mode-hook 'ms-coffee)

;; C
(defun ms-c-common ()
  (setq c-basic-offset 4
        tab-width 4)
  (c-toggle-auto-state 1)
  (define-key c-mode-base-map (kbd "RET") 'indent-new-comment-line))
(add-hook 'c-mode-common-hook 'ms-c-common)

;; Lua
(defun ms-lua ()
  (setq lua-indent-level 4))
(add-hook 'lua-mode-hook 'ms-lua)

;; CSS
(defun ms-css-common ()
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'ms-css-common)

;; Email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(defun ms-mail ()
  (turn-on-auto-fill)
  (setq fill-column 72))
(add-hook 'mail-mode-hook 'ms-mail)
