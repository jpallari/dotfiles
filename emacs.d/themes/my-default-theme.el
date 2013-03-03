(deftheme my-default "My default theme")

(custom-theme-set-faces
 'my-default
 '(default ((((type graphic))
             (:background "white smoke" :foreground "black"))
            (((class color) (min-colors 8))
             (:background "black" :foreground "white"))
            (t
             (:inherit nil :stipple nil :background "unspecified-bg"
                       :foreground "unspecified-fg" :inverse-video nil :box nil
                       :strike-through nil :overline nil :underline nil :slant normal
                       :weight normal :height 1 :width normal :foundry "default"
                       :family "default"))))
 '(cursor ((((type graphic))
            (:background "black" :foreground "white"))
           (t
            (:background "white"))))
 '(highlight ((((class color) (min-colors 88) (background light))
               (:background "darkseagreen2"))
              (((class color) (min-colors 88) (background dark))
               (:background "gray14"))
              (((class color) (min-colors 16) (background light))
               (:background "darkseagreen2"))
              (((class color) (min-colors 16) (background dark))
               (:background "gray14"))
              (((class color) (min-colors 8))
               (:background "blue" :foreground "white"))
              (t
               (:inverse-video t))))
 '(widget-field ((((background light))
                  (:background "light gray" :foreground "black"))
                 (((background dark))
                  (:background "#3a3a3a" :foreground "#d7ff5f")))))

(provide-theme 'my-default)
