;;;; pkg-management.el -- package management and package specific settings

(require 'my-utils)

;; Run initialization on old versions
(when (<= emacs-major-version 23)
  (if (require 'package nil 'noerror)
      (package-initialize)))

;; Package list
(setq my-pkgs-alist
  '(("essential" expand-region win-switch paredit iedit)
    ("autocomplete" auto-complete ac-nrepl)
    ("apps" magit auctex)
    ("modes" markdown-mode erlang go-mode)
    ("clojure" clojure-mode cider)
    ("haskell" haskell-mode ghci-completion ghc)
    ("python" virtualenv flymake-python-pyflakes)
    ("webdev" js2-mode js-comint less-css-mode flymake-jshint)))

;; Repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Autoloads
(autoload 'ghc-init "ghc" "GHC completion." t)

;; Functions and commands
(defun jedi ()
  (interactive)
  (if (fboundp 'jedi-mode)
      (jedi-mode (if jedi-mode -1 1))
    (jedi:setup)
    (define-key jedi-mode-map (kbd "M-TAB") 'jedi:complete)
    (define-key jedi-mode-map (kbd "C-c C-d") 'jedi:show-doc)
    (define-key jedi-mode-map (kbd "C-c C-g") 'jedi:goto-definition)
    (define-key jedi-mode-map (kbd "C-c r") 'jedi:related-names)))

;; Eval after load
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
     (setq ac-auto-start nil)
     (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
     (ac-set-trigger-key "TAB")))

(eval-after-load "win-switch"
  '(progn
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
     (win-switch-add-key "i" 'split-horizontally)))

;; Hook functions
(defun pkg-after-init ()
  ;; iedit
  (when (fboundp 'iedit-mode)
    (global-set-key (kbd "M-N") 'iedit-mode))

  ;; auto-complete
  (when (require 'auto-complete-config nil 'noerror)
    (ac-config-default))

  ;; Keybindings
  (when (fboundp 'er/expand-region)
    (global-set-key (kbd "M-M") 'er/expand-region))
  (when (fboundp 'win-switch-dispatch)
    (global-set-key (kbd "C-x o") 'win-switch-dispatch))

  ;; Paredit
  (when (fboundp 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'ielm-mode-hook 'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'paredit-mode))

  ;; Automode
  (extend-auto-mode-alist 'markdown-mode "\\.markdown$" "\\.md$" "\\.text$")
  (extend-auto-mode-alist 'js2-mode "\\.js$")
  (extend-auto-mode-alist 'erlang-mode "\\.\\(e\\|h\\)rl$")
  (extend-auto-mode-alist 'clojure-mode "\\.clj$")

  ;; Aliases
  (defalias 'git-st 'magit-status)
  (defalias 'hg-st 'monky-status)

  ;; Theme
  (call-until-no-error `(load-theme my-default ,t)))

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
  (if (fboundp 'skewer-mode) (skewer-mode -1))
  (subword-mode)
  (define-key js2-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
  (define-key js2-mode-map (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (define-key js2-mode-map (kbd "C-c b") 'js-send-buffer)
  (define-key js2-mode-map (kbd "C-c C-b") 'js-send-buffer-and-go)
  (define-key js2-mode-map (kbd "C-c g") 'js-send-region)
  (define-key js2-mode-map (kbd "C-c C-g") 'js-send-region-and-go)
  (define-key js2-mode-map (kbd "C-c l") 'js-load-file-and-go))

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

(defun ms-haskell ()
  "Haskell hook function."
  (setq tab-width 4
        haskell-indent-offset 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        c-basic-offset 4)
  (define-key haskell-mode-map (kbd "C-c .") 'haskell-mode-format-imports)
  (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
  (define-key haskell-mode-map (kbd "C-m") 'newline)
  (subword-mode)
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (when (fboundp 'ghc-init) (ghc-init)))

(defun ms-haskell-ghci ()
  "Haskell GHCI hook function."
  (setq comint-prompt-regexp "^ghci> ")
  (when (require 'ghci-completion nil t)
    (turn-on-ghci-completion)
    (when (boundp 'ghc-merged-keyword)
      (setq pcomplete-command-completion-function
            (lambda () (pcomplete-here* ghc-merged-keyword))))))

(defun ms-tex ()
  "TeX hook function."
  (setq TeX-auto-save t
        TeX-PDF-mode t
        TeX-parse-self t
        LaTeX-verbatim-environments '("Verbatim" "lstlisting"))
  (add-to-list 'TeX-command-list
               '("Biber" "biber %s.bcf" TeX-run-BibTeX nil t)))

(defun ms-js-comint ()
  "JS comint hook function."
  (ansi-color-for-comint-mode-filter)
  (subword-mode)
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string "\e\\[[0-9]+[GKJ]" "" output)))
  (setq comint-process-echoes t))

(defun ms-go ()
  "Go hook function."
  (subword-mode)
  (setq tab-width 8
        indent-tabs-mode t))

;; Hooks
(add-hook 'after-init-hook 'pkg-after-init)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'js2-mode-hook 'ms-js2)
(add-hook 'magit-log-edit-mode-hook 'ms-magit)
(add-hook 'markdown-mode-hook 'ms-markdown)
(add-hook 'haskell-mode-hook 'ms-haskell)
(add-hook 'inferior-haskell-mode-hook 'ms-haskell-ghci)
(add-hook 'TeX-mode-hook 'ms-tex)
(add-hook 'go-mode-hook 'ms-go)
(setq inferior-js-mode-hook 'ms-js-comint)
