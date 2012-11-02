;;;; pkg-management.el -- package management and package specific settings

;; Run initialization on old versions
(when (<= emacs-major-version 23) ;; Emacs 23 and older
  (add-to-list 'load-path "~/.emacs.d/package/")
  (require 'package)
  (package-initialize))

;; Package list
(defconst my-pkgs-alist
  '(("essential" expand-region win-switch)
    ("apps" magit monky auctex w3m)
    ("modes" lua-mode markdown-mode erlang)
    ("clojure" clojure-mode nrepl)
    ("haskell" haskell-mode ghci-completion)
    ("python" python virtualenv flymake-python-pyflakes)
    ("webdev" js2-mode js-comint coffee-mode less-css-mode flymake-jshint flymake-coffee)))

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

;; After init function
(defun pkg-after-init ()
  ;; Keybindings
  (when (fboundp 'er/expand-region)
    (global-set-key (kbd "M-M") 'er/expand-region))
  (when (fboundp 'win-switch-dispatch)
    (global-set-key (kbd "C-x o") 'win-switch-dispatch))

  ;; Aliases
  (defalias 'git-st 'magit-status)
  (defalias 'hg-st 'monky-status))

;; win-switch
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

;; W3M
(unless (getenv "DISPLAY")
  (eval-after-load "w3m"
    '(progn
       (setq browse-url-browser-function 'w3m-browse-url
             w3m-use-cookies t)))
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

;; Hooks
(add-hook 'after-init-hook 'pkg-after-init)
