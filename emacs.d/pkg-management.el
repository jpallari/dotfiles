;;;; pkg-management.el -- package management and package specific settings

;; Version specific settings
(when (and (<= emacs-major-version 23) ;; Emacs 23 and older
           (require 'package nil t))
  (add-to-list 'load-path "~/.emacs.d/package/"))

;; Package list
(defconst my-pkgs-alist
  '(("essential" fill-column-indicator expand-region undo-tree)
    ("apps" magit monky auctex w3m)
    ("modes" lua-mode haskell-mode markdown-mode erlang)
    ("clojure" clojure-mode nrepl)
    ("python" python virtualenv flymake-python-pyflakes)
    ("webdev" js2-mode js-comint coffee-mode less-css-mode flymake-jshint flymake-coffee)))

;; Initialize
(when (fboundp 'package-initialize)
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Package management functions
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
  (mapc (lambda (pkglist) (mapc 'ask-to-install (cdr pkglist))) my-pkgs-alist))

;; Keybindings
(global-set-key (kbd "M-?") 'undo-tree-redo)
(global-set-key (kbd "M-M") 'er/expand-region)

;; Aliases
(defalias 'git-st 'magit-status)
(defalias 'hg-st 'monky-status)

;; Fill column indicator
(setq fci-rule-width 1
      fci-rule-color "#87005f"
      fci-rule-character-color "#87005f")

;; W3M
(unless (getenv "DISPLAY")
  (setq browse-url-browser-function 'w3m-browse-url
        w3m-use-cookies t
        w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))
