;;;; pkg-management.el -- package management and package specific settings

;; Run initialization on old versions
(when (<= emacs-major-version 23)
  (if (require 'package nil 'noerror)
      (package-initialize)))

;; Package list
(defconst my-pkgs-alist
  '(("essential" expand-region win-switch)
    ("autocomplete" auto-complete jedi auto-complete-clang ac-nrepl ac-js2)
    ("apps" magit monky auctex)
    ("modes" lua-mode markdown-mode erlang)
    ("clojure" clojure-mode nrepl)
    ("haskell" haskell-mode ghci-completion)
    ("python" virtualenv flymake-python-pyflakes)
    ("webdev" js2-mode js-comint less-css-mode flymake-jshint)))

;; Repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Package management functions and commands
(defun ask-to-install (pkg)
  (and (not (package-installed-p pkg))
       (y-or-n-p (format "Package %s is not installed. Install it? " pkg))
       (package-install pkg)))

(defun install-missing-packages (pkg-list)
  "Installs all the missing packages from selected list."
  (interactive (list (completing-read "Choose a package group: " my-pkgs-alist)))
  (mapc 'ask-to-install (cdr (assoc pkg-list my-pkgs-alist))))

(defun install-missing-packages-all ()
  "Installs all the missing packages from every package list."
  (interactive)
  (mapc
   (lambda (pkglist)
     (mapc 'ask-to-install (cdr pkglist)))
   my-pkgs-alist))

;; Autoloads
(autoload 'ghc-init "ghc" "GHC completion." t)

;; Automode
(setq auto-mode-alist
      (append
       '(("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.js$" . js2-mode)
         ("\\.\\(e\\|h\\)rl$" . erlang-mode)
         ("\\.clj$" . clojure-mode))
       auto-mode-alist))

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
  ;; auto-complete
  (when (require 'auto-complete-config nil 'noerror)
    (ac-config-default))

  ;; Keybindings
  (when (fboundp 'er/expand-region)
    (global-set-key (kbd "M-M") 'er/expand-region))
  (when (fboundp 'win-switch-dispatch)
    (global-set-key (kbd "C-x o") 'win-switch-dispatch))

  ;; Aliases
  (defalias 'git-st 'magit-status)
  (defalias 'hg-st 'monky-status))

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
(add-hook 'after-init-hook 'pkg-after-init)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'js2-mode-hook 'ms-js2)
(add-hook 'magit-log-edit-mode-hook 'ms-magit)
(add-hook 'markdown-mode-hook 'ms-markdown)
(add-hook 'haskell-mode-hook 'ms-haskell)
(add-hook 'inferior-haskell-mode-hook 'ms-haskell-ghci)
(add-hook 'coffee-mode-hook 'ms-coffee)
(add-hook 'TeX-mode-hook 'ms-tex)
(setq inferior-js-mode-hook 'ms-js-comint)

(add-hook 'lua-mode-hook
          (lambda ()
            (setq lua-indent-level 4)))
