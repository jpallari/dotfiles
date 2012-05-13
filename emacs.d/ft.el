;; Filetype specific settings for Emacs


;; Automode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("dotfiles\\/emacs$" . lisp-mode))
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; LISP
(defun ft-lisp ()
  (eldoc-mode 1))
(add-hook 'lisp-mode-hook 'ft-lisp)

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
  (setq tab-width 4
        c-basic-offset 4
        py-indent-offset 4
        python-indent-offset 4
        fill-column 79))
(add-hook 'python-mode-hook 'ft-python)

;; Haskell
(defun ft-haskell ()
  (setq tab-width 2
        c-basic-offset 2)
  (turn-on-haskell-indent))
(add-hook 'haskell-mode-hook 'ft-haskell)

;; CoffeeScript
(defun ft-coffee ()
  (setq tab-width 2
        c-basic-offset 2
        coffee-js-mode 'js-mode)
  (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer))
(add-hook 'coffee-mode-hook 'ft-coffee)

