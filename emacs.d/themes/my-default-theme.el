(deftheme my-default "My default theme")

(let ((gui-class '((type graphic)))
      (8-color-class '((class color) (min-colors 8)))
      (light-88-class '((class color) (min-colors 88) (background light)))
      (dark-88-class '((class color) (min-colors 88) (background dark)))
      (light-16-class '((class color) (min-colors 16) (background light)))
      (dark-16-class '((class color) (min-colors 16) (background dark)))
      (light-class '((background light)))
      (dark-class '((background dark)))

      ;; Colors
      (light-bg "white")
      (light-fg "black")
      (dark-bg "black")
      (dark-fg "white"))

  (custom-theme-set-faces
   'my-default
   `(default
      ((,gui-class
        (:background ,light-bg :foreground ,light-fg))
       (,8-color-class
        (:background ,dark-bg :foreground ,dark-fg))
       (t
        (:inherit nil :stipple nil :background "unspecified-bg"
                  :foreground "unspecified-fg" :inverse-video nil
                  :box nil :strike-through nil :overline nil
                  :underline nil :slant normal :weight normal
                  :height 1 :width normal
                  :foundry "default" :family "default"))))
   `(cursor
     ((,gui-class
       (:background "black" :foreground "white"))
      (t
       (:background "white"))))
   `(highlight
     ((,light-class
       (:background "#c1ffc1"))
      (,dark-class
       (:background "#242424"))
      (,8-color-class
       (:background "blue" :foreground "white"))
      (t
       (:inverse-video t))))
   `(widget-field
     ((,light-class
       (:background "light gray" :foreground "black"))
      (,dark-class
       (:background "#3a3a3a" :foreground "#d7ff5f"))))

   ;; Org
   `(org-document-title
     ((,gui-class (:weight bold :height 1.5))))
   `(org-document-info
     ((,gui-class (:weight bold))))
   `(org-agenda-date-today
     ((,gui-class (:slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,gui-class (:inherit font-lock-comment-face))))
   `(org-archived
     ((,gui-class (:slant italic))))
   `(org-checkbox
     ((,gui-class (:box (:line-width 1 :style released-button)))))
   `(org-date
     ((,gui-class (:underline t))))
   `(org-done
     ((,gui-class (:bold t :weight bold :box (:line-width 1 :style none)))))
   `(org-todo
     ((,gui-class (:bold t :weight bold :box (:line-width 1 :style none)))))
   `(org-level-1
     ((,gui-class (:height 1.3))))
   `(org-level-2
     ((,gui-class (:height 1.2))))
   `(org-level-3
     ((,gui-class (:height 1.1))))
   `(org-link
     ((,gui-class (:underline t))))
   `(org-tag
     ((,gui-class (:bold t :weight bold))))
   `(org-column-title
     ((,gui-class (:underline t :weight bold))))
   ))

(provide-theme 'my-default)
