(deftheme my-theme
  "Created 2014-01-01.")

(custom-theme-set-variables
 'my-theme
 )

(custom-theme-set-faces
 'my-theme
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "orange" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(bold ((t (:weight bold))))
 '(cursor ((t (:background "grey"))))
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-default-highlight-face ((t (:background "medium blue"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-history-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-method-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-source-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-tag-header-face ((t (:background "dark green"))))
 '(error ((t (:foreground "red1" :weight thin))))
 '(region ((t (:background "indian red")))))

(provide-theme 'my-theme)
