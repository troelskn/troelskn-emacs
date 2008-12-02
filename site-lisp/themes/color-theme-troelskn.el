(defun color-theme-robin-hood-troelskn ()
  ""
  (interactive)
  (color-theme-gnome2)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-robin-hood-troelskn
       (
        (foreground-color . "navajo white")
        (background-color . "#304020"))
       (
        (CUA-mode-read-only-cursor-color . "white")
        (help-highlight-face . info-xref)
        (list-matching-lines-buffer-name-face . bold))
       (default ((t (nil))))
       (button ((t (:bold t))))
       (calendar-today-face ((t (:foreground "lemon chiffon"))))
       (custom-button-face ((t (:bold t :foreground "DodgerBlue1"))))
       (diary-face ((t (:bold t :foreground "yellow"))))
       (fringe ((t (:background "#CCC"))))
       (header-line ((t (:background "#030" :foreground "#AA7"))))
       (holiday-face ((t (:bold t :foreground "peru"))))
       (ido-subdir-face ((t (:foreground "MediumSlateBlue"))))
       (isearch ((t (:foreground "pink" :background "red"))))
       (isearch-lazy-highlight-face ((t (:foreground "red"))))
       (menu ((t (:background "#304020" :foreground "navajo white"))))
       (minibuffer-prompt ((t (:foreground "pale green"))))
       (modeline ((t (:background "dark olive green" :foreground "wheat" :box (:line-width 1 :style released-button)))))
       (mode-line-inactive ((t (:background "dark olive green" :foreground "khaki" :box (:line-width 1 :style released-button)))))
       (semantic-dirty-token-face ((t (:background "grey22"))))
       (tool-bar ((t (:background "#304020" :foreground "wheat" :box (:line-width 1 :style released-button)))))
       (tooltip ((t (:background "lemon chiffon" :foreground "black"))))

       (tabbar-default ((t (:height 110 :family "unknown-dejavu sans mono" :background "dark olive green"))))
       (tabbar-highlight ((t (:underline t))))
       (tabbar-selected ((t (:height 110 :background "#304020" :foreground "khaki" :color "khaki" :bold nil :box (:line-width 1 :style pressed-button)))))
       (tabbar-unselected ((t (:height 110 :background "dark olive green" :foreground "wheat" :bold nil :box (:line-width 1 :style released-button)))))
       (tabbar-button ((t (:height 110 :background "dark olive green" :foreground "wheat" :bold nil :box (:line-width 1 :style released-button)))))

       (hl-line ((t (:background "dark slate blue"))))
       (font-lock-string-face ((t (:background "#3F542A" :foreground "beige"))))
       (flymake-errline ((t (:background "#800"))))

))))

(defun color-theme-charcoal-black-troelskn ()
  ""
  (interactive)
  (color-theme-charcoal-black)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-charcoal-black-troelskn
       (default ((t (nil))))

       (modeline ((t (:background "#CCCCCC" :foreground "#333333" :box (:line-width 1 :style released-button)))))
       (mode-line-inactive ((t (:background "#CCCCCC" :foreground "#888888" :box (:line-width 1 :style released-button)))))
       (mode-line-buffer-id ((t (:bold t :background "#CCCCCC" :foreground "#333333" :weight bold))))

       (tabbar-default ((t (:height 110 :family "unknown-dejavu sans mono" :background "#CCC"))))
       (tabbar-highlight ((t (:underline nil))))
       (tabbar-selected ((t (:height 110 :background "#333333" :foreground "beige" :bold nil :color "beige" :box (:line-width 1 :style pressed-button)))))
       (tabbar-unselected ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))
       (tabbar-button ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))

       (highlight ((t (:background "dark slate blue" :foreground "light blue"))))
       (hl-line ((t (:background "#373250"))))
       (font-lock-string-face ((t (:foreground "#A6A" :background "#422842"))))
       (flymake-errline ((t (:background "#800"))))

))))



(defun color-theme-jonadabian-slate-troelskn ()
  "todo:
current-line
string-background
"
  (interactive)
  (color-theme-jonadabian-slate)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-jonadabian-slate-troelskn

       (font-lock-comment-delimiter-face ((t (:slant normal))))
       (font-lock-comment-face ((t (:slant normal))))

       (tabbar-default ((t (:height 110 :family "unknown-dejavu sans mono" :background "#007080"))))
       (tabbar-highlight ((t (:underline t))))
       (tabbar-selected ((t (:height 110 :background "#305050" :foreground "cyan" :color "cyan" :bold nil :box (:line-width 1 :style pressed-button)))))
       (tabbar-unselected ((t (:height 110 :background "#007080" :foreground "cyan" :color "cyan" :bold nil :box (:line-width 1 :style released-button)))))
       (tabbar-button ((t (:height 110 :background "#007080" :foreground "cyan" :color "cyan" :bold nil :box (:line-width 1 :style released-button)))))

       (hl-line ((t (:background "#76558E"))))
       (flymake-errline ((t (:background "#800"))))

))))


