;; troels' dot-emacs-file

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq default-tab-width 2)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 1)
(add-to-list 'default-frame-alist
             '(cursor-type . bar))
(setq make-backup-files nil)
(setq line-number-mode t)
;(setq abbrev-mode nil)
(setq show-trailing-whitespace t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq scroll-step 1)
(setq scroll-conservatively 10000)

(require 'iso-transl)
(setq inhibit-field-text-motion t)
(setq x-select-enable-clipboard t)
(put 'downcase-region 'disabled nil)

;;;(prefer-coding-system 'utf-8)

;; font-lock settings
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; (when (window-system)
;;   ;;(set-face-background 'font-lock-builtin-face "#333")
;;   (set-face-foreground 'font-lock-comment-face "#008000")
;;   (set-face-foreground 'font-lock-constant-face "#000080")
;;   ;;(set-face-background 'font-lock-doc-face "#333")
;;   (set-face-foreground 'font-lock-function-name-face "#333333")
;;   (set-face-foreground 'font-lock-keyword-face "#0000FF")
;;   ;;(set-face-background 'font-lock-preprocessor-face "#333")
;;   (set-face-foreground 'font-lock-string-face "#008080")
;;   (set-face-background 'font-lock-string-face "#EDF8FF")
;;   (set-face-foreground 'font-lock-type-face "#333333")
;;   ;;(set-face-background 'font-lock-variable-name-face "#333")
;;   (set-face-foreground 'font-lock-warning-face "#000000")
;;   (set-face-attribute  'font-lock-warning-face nil :weight 'normal))

;; (when (not (window-system))
;;   (set-background-color "Black")
;;   (set-foreground-color "White")
;;   (set-cursor-color "LightSkyBlue")
;;   (set-mouse-color "LightSkyBlue"))

;; color-theme
(require 'color-theme)
(color-theme-initialize)

;; customise matching parens
(require 'paren)
;; (set-face-background 'show-paren-match-face "#FFFFCC")
;; (set-face-foreground 'show-paren-match-face "#FF0000")
;; (set-face-attribute 'show-paren-match-face nil :weight 'normal)

;; vim-style % -> matching paren
(global-set-key "\M-2" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
      ((save-excursion
         (backward-char 1)
         (looking-at "\\s\)"))
       (backward-list 1))))

(global-set-key "\M-\"" 'mark-sexp-at-point)
(defun mark-sexp-at-point (arg)
  "Selects the sexp at point."
  (interactive "p")
  (if (save-excursion (backward-char 1) (looking-at "\\s\)"))
      (match-paren ()))
  (mark-sexp))

;; highlight current line
(global-hl-line-mode 1)
(autoload 'highlight-current-line "highlight-current-line")
;; (when (window-system)
;;   (set-face-background 'hl-line "#FFFFCC"))

;; textpad-mode
(require 'textpad)
(textpad-mode)

;; Tabbar
(require 'tabbar)
(tabbar-mode)

;; @todo bind this to a shortcut, or perhaps a button on the tabbar
(defun tabbar-buffer-groups-switch-mode ()
  "Switches mode between displaying all tabs, or by group"
  (setq tabbar-buffer-groups-function
        (if (eq tabbar-buffer-groups-function 'tabbar-buffer-groups)
          (lambda () (list "All buffers"))
          'tabbar-buffer-groups)))
(tabbar-buffer-groups-switch-mode)

;; (when (window-system)
;;;   (set-face-attribute  'tabbar-default nil :height 1)
;;;   (set-face-attribute  'tabbar-unselected nil :box '(:line-width 1 :color "white" :style released-button))
;;;   (set-face-attribute  'tabbar-selected nil :box '(:line-width 1 :color "white" :style pressed-button))
;;;   (set-face-foreground 'tabbar-selected "black")
;;;   (set-face-background 'tabbar-selected "white")
;;;   (set-face-foreground 'tabbar-selected "black")
;;;   (set-face-attribute  'tabbar-highlight nil :underline nil)
;;;   (set-face-font       'tabbar-default "Monospace-12"))


; old crap ...
;(set-face-font       'tabbar-default "-dejavu-dejavu sans mono-medium-r-normal--12-120-75-75-*-*-iso8859-1")
;; (global-set-key [(control shift tab)] 'tabbar-backward)
;; (global-set-key [(control shift iso-lefttab)] 'tabbar-backward)
;; (global-set-key [(control tab)]       'tabbar-forward)

;; compatibility with gnome terminal
(global-set-key [(control prior)] 'tabbar-backward)
(global-set-key [(control prior)] 'tabbar-backward)
(global-set-key [(control next)]  'tabbar-forward)


;;; Various key-bindings
(global-set-key "\M-g" 'goto-line)
;; (global-set-key [?\C-x?\C-b] 'buffer-menu)
(global-set-key '[67109053] 'comment-region)
(global-set-key '[67109031] 'uncomment-region)
(put 'upcase-region 'disabled nil)
(global-set-key [C-return] 'newline-and-indent)
(global-set-key [(backtab)] 'dabbrev-expand)

(iswitchb-mode)

(require 'lua-mode)

;; PHP mode
;(autoload 'php-mode "php-mode" "Major mode for PHP Scripts." t)
(require 'php-mode)

;; Use w3m to browse the php-manual
;(setq php-browse-url 'w3m-browse-url)

;; Turn on font lock when in PHP mode
(add-hook 'php-mode-hook
  'turn-on-font-lock)

;; fixed tabstop list
(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (nreverse ls)))

(defun indent-selected-region-or-line ()
  (interactive)
  (if mark-active
      (indent-region (min (mark) (point)) (max (mark) (point)))
    (indent-for-tab-command)))

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (defalias 'c-electric-backspace 'delete-backward-char)
;;;               (defalias 'c-indent-command 'indent-selected-region-or-line)
              (set (make-local-variable 'tab-stop-list)
                   (my-build-tab-stop-list tab-width))
              (setq c-basic-offset tab-width)
              (c-set-offset 'defun-block-intro tab-width)
              (c-set-offset 'arglist-intro tab-width)
              (c-set-offset 'arglist-close 0)
              (c-set-offset 'defun-close 0)
              (setq abbrev-mode nil)))

;; Set default types
(add-to-list 'auto-mode-alist
             '("\\.php$" . php-mode))

(add-hook 'python-mode-hook
      '(lambda ()
         "Turn off Indent Tabs mode."
         (set (make-local-variable 'indent-tabs-mode) nil)
         (set (make-local-variable 'python-indent) 4)))

;; Toggle between PHP & HTML Helper mode.  Useful when working on
;; php files, that can been intertwined with HTML code
(defun toggle-php-html-mode ()
  (interactive)
  "Toggle mode between PHP & HTML modes"
  (cond ((or (equal mode-name "XHTML") (equal mode-name '(sgml-xml-mode "XHTML" "HTML")))
         (php-mode))
        ((equal mode-name "PHP")
         (html-mode))))

(add-hook 'php-mode-hook
          #'(lambda ()
              (local-set-key '[f6] 'toggle-php-html-mode)))

(require 'autohtml)
(add-hook 'html-mode-hook
          #'(lambda ()
              (local-set-key '[f6] 'toggle-php-html-mode)
              (local-set-key "\M-+" 'autohtml-close-tag)))

;; Start server, to allow emacsclient to connect
;(server-start)

(require 'w3m-load)

;; auto-compile dot-emacs (moxley stratton)
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
                                                     default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

;; my-grep
;;; (setq *my-grep-history-filename* ()
;;;       *my-grep-history-search* ())

;;; (defun add-slashes (in-string)
;;;   (replace-regexp-in-string "\\(\\\\\\)" "\\\\\\\\"
;;;                             (replace-regexp-in-string "\\(\"\\)" "\\\\\"" in-string nil nil 1)
;;;                             nil nil 1))

;;; (defun my-grep (folder filename-pattern search-pattern)
;;;   "Proxy for external command find + grep

;;; Searches all files, recursively, from a folder.
;;; "
;;;   (interactive
;;;    (list
;;;     (read-file-name "Folder to search from: " (or (and buffer-file-name (file-name-directory buffer-file-name)) ""))
;;;     (read-string "Filename pattern: " ".*php" '*my-grep-history-filename*)
;;;     (read-string "Search pattern: " "" '*my-grep-history-search*)))
;;;   (message "Searching for: %s %s %s" folder filename-pattern search-pattern)
;;;   (let ((buffername "*grep*"))
;;;     (when (get-buffer buffername)
;;;       (kill-buffer buffername))
;;;     (save-excursion
;;;       (switch-to-buffer buffername)
;;;       (message (format "find %s -regex \"%s\" | xargs grep -EnH \"%s\""
;;;                              (shell-quote-argument (expand-file-name folder))
;;;                              (add-slashes filename-pattern)
;;;                              (add-slashes search-pattern)))
;;;       (shell-command (format "find %s -regex \"%s\" | xargs grep -EnH \"%s\""
;;;                              (shell-quote-argument (expand-file-name folder))
;;;                              (add-slashes filename-pattern)
;;;                              (add-slashes search-pattern)) buffername)
;;;       (grep-mode))))

;;; (global-set-key '[f5] 'my-grep)

;; Define function documentation function
(defun php-manual-lookup ()
  "Shows short documentation for the word at the point."
  (interactive)
;;   (shell-command (format "phpm %s"
;;                          (shell-quote-argument (current-word t)))))
  (let ((word (current-word t))
        (buffername "*phpm*"))
    (when (get-buffer buffername)
      (kill-buffer buffername))
    (save-excursion
      (pop-to-buffer buffername)
      (shell-command (format "phpm %s"
                             (shell-quote-argument word)) buffername)
      (other-window 1))))
(global-set-key '[f4] 'php-manual-lookup)

(require 'php-tokens)
(global-set-key '[f7] 'php-tokens)

;; php-lint + phpsat
(defun php-lint ()
  "Lint PHP file for syntax errors"
  (interactive)
  (let* ((source-buffername (buffer-name))
         (buffername "*php-lint*")
         (filename (buffer-file-name)))
    (when (get-buffer buffername)
      (kill-buffer buffername))
    (save-excursion
      (pop-to-buffer buffername)
      (shell-command (format "if php -l %s; then phpsat %s; fi"
                             (shell-quote-argument filename) (shell-quote-argument filename)) buffername buffername))))

(defun revert-utf8 ()
  "Reverts the current buffer to utf-8"
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))

(global-set-key '[f5] 'php-lint)

(global-set-key [f8] 'other-window)
(global-set-key [f9] 'delete-other-windows)
(global-set-key [f12] 'revert-utf8)

;; scheme-mode
(require 'quack)
(setq quack-pretty-lambda-p t)
(set-face-foreground 'quack-pltish-paren-face "#888888")
(set-face-foreground 'quack-pltish-keyword-face "#0000FF")
(set-face-attribute 'quack-pltish-keyword-face nil :weight 'normal)
(set-face-foreground 'quack-pltish-defn-face "#CC9900")
(set-face-attribute 'quack-pltish-defn-face nil :weight 'normal)
(set-face-foreground 'quack-pltish-comment-face "#008000")
(set-face-foreground 'quack-pltish-selfeval-face "#008080")
(set-face-background 'quack-pltish-selfeval-face "#EDF8FF")

;; Add color to a shell running in emacs M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; rails-mode
(require 'snippet)
(require 'find-recursive)
(setq load-path (cons "/home/tkn/public/emacs-ruby-mode" load-path))
(require 'ruby-mode)
(setq load-path (cons "/home/tkn/public/emacs-rails" load-path))
(require 'rails)
(define-key rails-minor-mode-map (kbd "C-c C-RET") 'rails-goto-file-on-current-line)
(define-key rails-minor-mode-map [C-return] 'newline-and-indent)
(ruby-electric-mode)
(add-hook 'ruby-mode-hook
          #'(lambda ()
              ;(ruby-electric-mode nil)
              (local-set-key [tab] 'indent-selected-region-or-line)))



(add-hook 'after-init-hook
          (lambda ()
            (color-theme-charcoal-black)))
(color-theme-charcoal-black)


;; snippets
;; (defun snippet-getter-setter (column)
;;   "Create PHP getter/setters"
;;   (interactive "sColumn name: ")
;;   (insert
;;    (shell-command-to-string
;;     (format "/home/tkn/my/scripts/getter-setter.php %s" (shell-quote-argument column))))
;; )

;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "/home/tkn/external/yasnippets-rails/rails-snippets")
