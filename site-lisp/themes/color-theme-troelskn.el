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
       nil
       (default ((t (nil))))

       (modeline ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :box (:line-width 1 :style released-button)))))
       (mode-line-inactive ((t (:background "#CCCCCC" :foreground "#888888" :box (:line-width 1 :style released-button)))))
       (mode-line-buffer-id ((t (:height 110 :bold t :background "#CCCCCC" :foreground "#333333" :weight bold))))
       ;(minibuffer-prompt ((t (:height 220))))

       (tabbar-default ((t (:height 110 :family "unknown-dejavu sans mono" :background "#CCC"))))
       (tabbar-highlight ((t (:underline nil))))
       (tabbar-selected ((t (:height 110 :background "#333333" :foreground "beige" :bold nil :color "beige" :box (:line-width 1 :style pressed-button)))))
       (tabbar-unselected ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))
       (tabbar-button ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))

       (highlight ((t (:background "dark slate blue"))))
       (hl-line ((t (:background "#373250"))))
       (font-lock-string-face ((t (:foreground "#A6A" :background "#422842"))))
       (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))

       (quack-pltish-comment-face ((t (:foreground "#888a85"))))
       (quack-pltish-defn-face ((t (:foreground "#8ae234"))))
       (quack-pltish-keyword-face ((t (:foreground "#729fcf" :bold t))))
       (quack-pltish-paren-face ((t (:foreground "#666666"))))
       (quack-pltish-selfeval-face ((t (:foreground "#ad7fa8"))))

       (quack-threesemi-semi-face ((t (:foreground "#ccc" :background nil :bold t))))
       (quack-threesemi-text-face ((t (:foreground "#ccc" :background nil :bold t))))


       ;; tango begin
;;;        (font-lock-builtin-face ((t (:foreground "#729fcf"))))
;;;        (font-lock-comment-face ((t (:foreground "#888a85"))))
;;;        (font-lock-constant-face ((t (:foreground "#8ae234"))))
;;;        (font-lock-doc-face ((t (:foreground "#888a85"))))
;;;        (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
;;;        (font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
;;;        (font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
;;;        (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
;;;        (font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
;;;        (font-lock-function-name-face ((t (:foreground "#edd400" :bold t :italic t))))
       ;; tango end

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
       nil
       (default ((t (nil))))

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


;;; Color theme based on Tango Palette. Created by danranx@gmail.com
(defun color-theme-tango ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   '(color-theme-tango
     ((background-color . "#2e3436")
      (background-mode . dark)
      (border-color . "#888a85")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "#8ae234"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (border ((t (:background "#888a85"))))
     (fringe ((t (:background "grey10"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#555753"))))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-constant-face ((t (:foreground "#8ae234"))))
     (font-lock-doc-face ((t (:foreground "#888a85"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
     (font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
     (font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "#ffffff"))))
     (font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
     (font-lock-function-name-face ((t (:foreground "#edd400" :bold t :italic t))))
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
     (isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
     (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
     (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
     )))

(defun color-theme-scintilla-troelskn ()
  ""
  (interactive)
  (color-theme-scintilla)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-scintilla-troelskn
       nil
       (default ((t (nil))))

       (modeline ((t (:background "#CCCCCC" :foreground "#333333" :box (:line-width 1 :style released-button)))))
       (mode-line-inactive ((t (:background "#CCCCCC" :foreground "#888888" :box (:line-width 1 :style released-button)))))
       (mode-line-buffer-id ((t (:bold t :background "#CCCCCC" :foreground "#333333" :weight bold))))

       (tabbar-default ((t (:height 110 :family "unknown-dejavu sans mono" :background "#CCC"))))
       (tabbar-highlight ((t (:underline nil))))
       (tabbar-selected ((t (:height 110 :background "#333333" :foreground "beige" :bold nil :color "beige" :box (:line-width 1 :style pressed-button)))))
       (tabbar-unselected ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))
       (tabbar-button ((t (:height 110 :background "#CCCCCC" :foreground "#333333" :bold nil :box (:line-width 1 :style released-button)))))

;;;        (highlight ((t (:background "dark slate blue"))))
       (hl-line ((t (:background "#DDDDDD"))))
       (font-lock-string-face ((t (:foreground "#FF39FF" :background "#FFE3FF" :weight normal))))
;;;        (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))

;;;        (quack-pltish-comment-face ((t (:foreground "#888a85"))))
;;;        (quack-pltish-defn-face ((t (:foreground "#8ae234"))))
;;;        (quack-pltish-keyword-face ((t (:foreground "#729fcf" :bold t))))
;;;        (quack-pltish-paren-face ((t (:foreground "#666666"))))
;;;        (quack-pltish-selfeval-face ((t (:foreground "#ad7fa8"))))

;;;        (quack-threesemi-semi-face ((t (:foreground "#ccc" :background nil :bold t))))
;;;        (quack-threesemi-text-face ((t (:foreground "#ccc" :background nil :bold t))))


       (flymake-errline ((t (:background "#800"))))

))))
