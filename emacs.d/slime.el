;;;; slime.el -- SLIME related settings

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq slime-protocol-version 'ignore
      inferior-lisp-program "sbcl --noinform --no-linedit"
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-when-complete-filename-expand t
      slime-truncate-lines nil)
