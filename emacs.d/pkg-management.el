;;;; pkg-management.el -- package management and package specific settings

;; Version specific settings
(when (and (<= emacs-major-version 23) ;; Emacs 23 and older
           (require 'package nil t))
  (add-to-list 'load-path "~/.emacs.d/package/"))

;; Package list
(setq my-pkgs-alist
      '(("essential" . (fill-column-indicator expand-region undo-tree))
        ("apps" . (magit auctex w3m))
        ("modes" . (lua-mode haskell-mode markdown-mode erlang))
        ("webdev" . (js2-mode js-comint coffee-mode less-css-mode flymake-jshint flymake-coffee))))

;; Initialize
(when (fboundp 'package-initialize)
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Package management functions
(defun install-missing-packages (pkg-list)
  "Installs all the missing packages from selected list."
  (interactive (list (completing-read "Choose a package group: " my-pkgs-alist)))
  (mapc (lambda (pkg)
          (or (package-installed-p pkg)
              (if (y-or-n-p (format "Package %s is missing. Install it? " pkg))
                  (package-install pkg))))
        (cdr (assoc pkg-list my-pkgs-alist))))

(defun install-missing-packages-all ()
  "Installs all the missing packages from every package list."
  (interactive)
  (mapc 'install-missing-packages (mapcar 'car my-pkgs-alist)))

;; Keybindings
(global-set-key (kbd "M-?") 'undo-tree-redo)
(global-set-key (kbd "M-M") 'er/expand-region)

;; Fill column indicator
(setq fci-rule-width 1
      fci-rule-color "#87005f"
      fci-rule-character-color "#87005f")
