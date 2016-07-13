;;;; fast-init.el -- init file to use when loading without site file

(package-initialize)
(load-file "~/.emacs.d/init.el")
(run-hooks 'after-init-hook)
