;;;; modesettings.el -- settings for different kinds of modes

;; Autoloads
(autoload 'ghc-init "ghc" "GHC completion." t)
(autoload 'slime "~/.emacs.d/my-slime" "Slime mode." t)
(autoload 'slime-mode "~/.emacs.d/my-slime" "Slime mode." t)

;; Automode
(setq auto-mode-alist
      (append
       '(("dotfiles\\/emacs$" . emacs-lisp-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.js$" . js2-mode)
         ("\\.\\(e\\|h\\)rl$" . erlang-mode)
         ("\\.clj$" . clojure-mode))
       auto-mode-alist))

;; Hook functions

(defun ms-js2 ()
  "JavaScript (JS2) hook function."
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

(defun ms-magit ()
  "Magit hook function."
  (setq fill-column 72)
  (turn-on-auto-fill))

(defun ms-markdown ()
  "Markdown hook function."
  (turn-on-auto-fill)
  (setq tab-width 4
        c-basic-offset 4
        fill-column 79
        whitespace-line-column 79))

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
  (eldoc-mode 1)
  (whitespace-mode 1)
  (local-set-key (kbd "RET") 'newline)
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        whitespace-line-column 79
        fill-column 79))

(defun ms-haskell ()
  "Haskell hook function."
  (setq tab-width 2
        haskell-indent-offset 2
        c-basic-offset 2)
  (define-key haskell-mode-map (kbd "C-c =") 'haskell-indent-insert-equal)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-c .") 'haskell-mode-format-imports)
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (when (fboundp 'ghc-init) (ghc-init)))

(defun ms-haskell-ghci ()
  "Haskell GHCI hook function."
  (when (require 'ghci-completion nil t)
    (turn-on-ghci-completion)
    (when (boundp 'ghc-merged-keyword)
      (setq pcomplete-command-completion-function
            (lambda () (pcomplete-here* ghc-merged-keyword))))))

(defun ms-coffee ()
  "CoffeeScript hook function."
  (make-local-variable 'tab-width)
  (setenv "NODE_NO_READLINE" "1")
  (whitespace-mode 1)
  (setq coffee-tab-width 2
        tab-width 2)
  (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)
  (define-key coffee-mode-map (kbd "C-m") 'newline))

(defun ms-tex ()
  "TeX hook function."
  (setq TeX-auto-save t
        TeX-PDF-mode t
        TeX-parse-self t)
  (add-to-list 'TeX-command-list
               '("Biber" "biber %s.bcf" TeX-run-BibTeX nil t)))

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

(defun ms-js-comint ()
  "JS comint hook function."
  (ansi-color-for-comint-mode-on)
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string
      ".*1G\.\.\..*5G" "..."
      (replace-regexp-in-string ".*1G.*3G" "> " output)))))

;; Hooks
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2 tab-width 2 c-basic-offset 2)))
(add-hook 'js2-mode-hook 'ms-js2)
(add-hook 'magit-log-edit-mode-hook 'ms-magit)
(add-hook 'markdown-mode-hook 'ms-markdown)
(add-hook 'rst-mode-hook 'ms-rst)
(add-hook 'python-mode-hook 'ms-python)
(add-hook 'haskell-mode-hook 'ms-haskell)
(add-hook 'inferior-haskell-mode-hook 'ms-haskell-ghci)
(add-hook 'coffee-mode-hook 'ms-coffee)
(add-hook 'c-mode-common-hook (lambda () (setq c-basic-offset 4 tab-width 4)))
(add-hook 'lua-mode-hook (lambda () (setq lua-indent-level 4)))
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'TeX-mode-hook 'ms-tex)
(add-hook 'mail-mode-hook 'ms-mail)
(add-hook 'org-mode-hook 'ms-org)
(add-hook 'comint-mode-hook 'ms-comint)
(setq inferior-js-mode-hook 'ms-js-comint)
