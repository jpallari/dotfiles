;;;; my-slime.el -- SLIME related settings

(load (expand-file-name "~/.quicklisp/slime-helper.el") t t t)
(require 'slime)

;; Hooks
(add-hook 'lisp-mode-hook 'slime-autodoc-mode)
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-t") 'slime-complete-symbol)))

;; Settings
(setq slime-protocol-version 'ignore
      inferior-lisp-program "sbcl --noinform --no-linedit"
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-when-complete-filename-expand t
      slime-truncate-lines nil)
