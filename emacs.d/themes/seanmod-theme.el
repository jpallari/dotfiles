(deftheme seanmod "")

(custom-theme-set-faces
 'seanmod
 
 '(default ((t (:background nil :foreground "#dadada"))))

 '(cursor ((t (:foreground "#ffcc22"))))
 '(region ((t (:background "#0000ff"))))
 '(hl-line ((t (:background "#262626"))))

 '(mode-line ((t (:background "#0000ee" :foreground "#ffffff" :box (:line-width 1 :color "#0000ff")))))
 '(mode-line-inactive ((t (:background "#00005f" :foreground "#767676" :box (:line-width 1 :color "#0000ee")))))

 '(minibuffer-prompt ((t (:foreground "#afffff" :weight bold))))
 '(ido-first-match ((t (:foreground "#ffff00"))))
 '(ido-subdir ((t (:foreground "#87d75f"))))

 '(font-lock-builtin-face ((t (:foreground "#ffff87" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#8a8a8a")))) 
 '(font-lock-constant-face ((t (:foreground "#ff8787"))))
 '(font-lock-function-name-face ((t (:foreground "#ffff87"))))
 '(font-lock-keyword-face ((t (:foreground "#5fd7ff"))))
 '(font-lock-string-face ((t (:foreground "#87d75f"))))
 '(font-lock-type-face ((t (:foreground "#ffff87"))))
 '(font-lock-variable-name-face ((t (:foreground "#ffff87" :weight bold))))
 '(font-lock-warning-face ((t (:foreground "#ff8700" :weight bold))))
 '(font-lock-doc-string-face ((t (:foreground "#8a8a8a")))) 

 '(link ((t (:underline t))))
 '(link-visited ((t (:underline t))))
 '(button ((t (:underline t))))

 '(widget-field ((t (:foreground "#d7ff5f" :background "#262626"))))
 )


(provide-theme 'seanmod)
