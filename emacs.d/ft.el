; Filetype specific settings for Emacs


; Automode
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))

; JavaScript
(setq js-indent-level 2)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq tab-width 2)))

; Markdown
(add-hook 'markdown-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq tab-width 4)
             (setq fill-column 79)))

; Python
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq tab-width 4)
             (setq fill-column 79)))

; Haskell
(add-hook 'haskell-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (turn-on-haskell-indent)))

; CoffeeScript
(add-hook 'coffee-mode-hook
          '(lambda ()
             (setq tab-width 2)))

