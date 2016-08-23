;;;; pkg-management.el -- package management and package specific settings

(require 'my-utils)

;;; Initialize packages
(package-initialize)

;;; Package list
(setq my-pkgs-list
      '(use-package
        expand-region
        win-switch
        paredit
        iedit
        ag
        auto-complete
        magit
        markdown-mode
        clojure-mode cider
        scala-mode sbt-mode
        haskell-mode
        js2-mode js-comint))

;;; Repos
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://stable.melpa.org/packages/")))

;;; use-package
(if (require 'use-package nil 'noerror)
    (progn
      (require 'diminish nil 'noerror)
      (require 'bind-key nil 'noerror))
  (progn
    (display-warning :warning "use-package not found")
    (defmacro use-package (&rest args) nil)))

(use-package expand-region
  :bind ("M-M" . er/expand-region))

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
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode))

(use-package iedit
  :bind ("M-N" . iedit-mode))

(use-package auto-complete
  :defer t
  :bind (:map ac-mode-map
              ("M-TAB" . auto-complete))
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (ac-config-default))

(use-package magit
  :defer t
  :init
  (defalias 'git-st 'magit-status)
  :config
  (setq fill-column 72)
  (turn-on-auto-fill))

(use-package markdown-mode
  :mode
  "\\.markdown\\'" "\\.md\\'" "\\.text\\'"
  :config
  (setq tab-width 4
        c-basic-offset 4
        whitespace-line-column 79))

(use-package clojure-mode
  :mode "\\.clj\\'")

(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c ." . haskell-mode-format-imports)
              ("C-j" . haskell-newline-and-indent)
              ("C-m" . newline))
  :config
  (setq tab-width 4
        haskell-indent-offset 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        c-basic-offset 4)
  (subword-mode)
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode))

(use-package AucTex
  :defer t
  :config
  (setq TeX-auto-save t
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
  (setq tab-width 2
        c-basic-offset 2
        jshint-configuration-path (concat (getenv "HOME") "/.jshint.json")
        js2-consistent-level-indent-inner-bracket-p t
        js2-pretty-multiline-decl-indentation-p t
        js2-basic-offset 2
        js2-strict-inconsistent-return-warning nil
        inferior-js-program-command "node")
  (setq inferior-js-mode-hook
        (lambda ()
          (ansi-color-for-comint-mode-filter)
          (subword-mode)
          (add-to-list
           'comint-preoutput-filter-functions
           (lambda (output)
             (replace-regexp-in-string "\e\\[[0-9]+[GKJ]" "" output)))
          (setq comint-process-echoes t)))
  (subword-mode))

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))
