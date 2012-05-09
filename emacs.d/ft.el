;; Filetype specific settings for Emacs


;; Automode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("dotfiles\\/emacs$" . lisp-mode))
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; JavaScript
(setq js-indent-level 2)
(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-pretty-multiline-decl-indentation-p t)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq tab-width 2)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq tab-width 2)))

;; Markdown
(add-hook 'markdown-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq tab-width 4)
             (setq fill-column 79)))

;; Python
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq tab-width 4)
             (setq fill-column 79)))

;; Haskell
(add-hook 'haskell-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (turn-on-haskell-indent)))

;; CoffeeScript
(add-hook 'coffee-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq coffee-js-mode 'js-mode)
             (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-buffer)))

