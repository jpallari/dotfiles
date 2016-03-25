;; Variables and constants
(defvar my-vendor-path "~/.emacs.d/vendor" "Path to vendor packages")
(defvar my-load-files '() "List of files to load during start up.")
(defvar my-keybindings-alist '() "List of keybindings")
(defvar my-aliases-alist '() "List of aliases")
(defvar my-pkgs-alist '() "List of packages to install")
(defvar my-clipboard-copy "xsel -b -i" "Shell clipboard copy command")
(defvar my-clipboard-paste "xsel -b -o" "Shell clipboard paste command")

(defconst ido-decorations-horizontal
  '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]"
    " [Too big]" " [Confirm]")
  "Ido decorations for horizontal listing.")

(defconst ido-decorations-vertical
  '("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]"
    " [Not readable]" " [Too big]" " [Confirm]")
  "Ido decorations for vertical listing.")

;; Functions
(defun filter-list (condp lst)
  "Passes each element in LST to CONDP, and filters out the
elements where the CONDP result is nil."
  (delq nil
        (mapcar (lambda (x)
                  (and (funcall condp x) x))
                lst)))

(defun set-my-keybindings ()
  "Sets keybindings according to `my-keybindings-alist'"
  (mapc (lambda (x)
          (global-set-key (read-kbd-macro (car x)) (cdr x)))
        my-keybindings-alist))

(defun set-my-aliases ()
  "Sets aliases according to `my-aliases-alist'"
  (mapc (lambda (x)
          (defalias (car x) (cdr x)))
        my-aliases-alist))

(defun load-my-load-files ()
  "Loads files according to `my-load-files'"
  (mapc (lambda (filename)
          (load filename t t t))
        my-load-files))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun string-ends-with (str ending)
  "Return non-nil if STR ends with ENDING."
  (string= (substring str (- 0 (length ending))) ending))

(defun subdirectories-of-directory (directory &optional full match nosort)
  "Gets a list of all the subdirectories in DIRECTORY. The
parameters FULL, MATCH, and NOSORT work the same as in
`directory-files-and-attributes`."
  (filter-list (lambda (file)
                  (and (eq (car (cdr file)) t)                 ; is directory
                       (not (string-ends-with (car file) ".")) ; isn't "."
                       (car file)))                            ; is not empty
                (directory-files-and-attributes directory full match nosort)))

(defun update-directory-loaddefs (directory)
  "Scans the autoloads from all the subdirectories of DIRECTORY,
and writes them to the loaddefs.el file of DIRECTORY"
  (let* ((generated-autoload-file (concat directory "/loaddefs.el"))
         (dirs-with-info (subdirectories-of-directory directory t))
         (dirs (mapcar 'car dirs-with-info)))
    (apply 'update-directory-autoloads dirs)))

(defun extend-auto-mode-alist (mode &rest patterns)
  "Extends `auto-mode-alist' with the given MODE and PATTERNS.
The MODE is applied for each pattern in PATTERNS only if MODE is
a bound function."
  (if (fboundp mode)
      (mapc (lambda (pattern)
              (add-to-list 'auto-mode-alist `(,pattern . ,mode)))
            patterns)))

(defun call-until-no-error (&rest expressions)
  "Call each expression in EXPRESSIONS until an expression does
not produce an error."
  (when expressions
    (let ((head (car expressions))
          (tail (cdr expressions)))
      (condition-case nil
          (if (listp head)
              (apply head)
            (funcall head))
        (error
         (apply 'call-until-no-error tail))))))

(defun ask-to-install (pkg)
  "Installs package PKG if it is not already installed and the
user answers yes to the prompt."
  (and (not (package-installed-p pkg))
       (y-or-n-p (format "Package %s is not installed. Install it? " pkg))
       (package-install pkg)))

;; Commands
(defun what-face (pos)
  "Displays the current face name under the cursor."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

(defun region-to-clipboard (start end)
  "Copies region contents to clipboard"
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-ring-save start end)
    (shell-command-on-region start end my-clipboard-copy))
  (message "Region copied to clipboard"))

(defun paste-from-clipboard ()
  "Pastes clipboard contents to buffer"
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string my-clipboard-paste))))

(defun ido-vertical (&optional arg)
  "Switches between vertical and horizontal style of listing in
IDO. Always switches to vertical style if ARG is non-nil."
  (interactive)
  (if (or arg (not (string= (substring (car ido-decorations) 0 1) "\n")))
      (progn ;; Set vertical
        (setq ido-decorations ido-decorations-vertical)
        (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))
    (progn ;; Set horizontal
      (setq ido-decorations ido-decorations-horizontal)
      (remove-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation))))

(defun update-vendor-loaddefs ()
  "Update loaddefs.el file for vendor directory."
  (interactive)
  (update-directory-loaddefs my-vendor-path))

(defun install-missing-packages (pkg-list)
  "Installs all the missing packages from selected list."
  (interactive (list (completing-read "Choose a package group: " my-pkgs-alist)))
  (mapc 'ask-to-install (cdr (assoc pkg-list my-pkgs-alist))))

(defun install-missing-packages-all ()
  "Installs all the missing packages from every package list."
  (interactive)
  (mapc
   (lambda (pkglist)
     (mapc 'package-install (cdr pkglist)))
   my-pkgs-alist))

(provide 'my-utils)
