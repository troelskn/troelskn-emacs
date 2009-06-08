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
(put 'upcase-region 'disabled nil)

;; open up some default buffers, so that they alwways appear in the same order
;;(get-buffer-create "TAGS")
(get-buffer-create "*Completions*")

;; some random utils
(require 'troelskn-utilities)

(prefer-coding-system 'utf-8)
(defun revert-utf8 ()
  "Reverts the current buffer to utf-8"
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))

;; font-lock settings
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; auto-compile dot-emacs (moxley stratton)
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
                                                     default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)

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

;; (set-face-background 'show-paren-match-face "#FFFFCC")
;; (set-face-foreground 'show-paren-match-face "#FF0000")
;; (set-face-attribute 'show-paren-match-face nil :weight 'normal)

;; (when (window-system)
;;   (set-face-background 'hl-line "#FFFFCC"))

;; (when (window-system)
;;;   (set-face-attribute  'tabbar-default nil :height 1)
;;;   (set-face-attribute  'tabbar-unselected nil :box '(:line-width 1 :color "white" :style released-button))
;;;   (set-face-attribute  'tabbar-selected nil :box '(:line-width 1 :color "white" :style pressed-button))
;;;   (set-face-foreground 'tabbar-selected "black")
;;;   (set-face-background 'tabbar-selected "white")
;;;   (set-face-foreground 'tabbar-selected "black")
;;;   (set-face-attribute  'tabbar-highlight nil :underline nil)
;;;   (set-face-font       'tabbar-default "Monospace-12"))

;; (set-face-foreground 'quack-pltish-paren-face "#888888")
;; (set-face-foreground 'quack-pltish-keyword-face "#0000FF")
;; (set-face-attribute 'quack-pltish-keyword-face nil :weight 'normal)
;; (set-face-foreground 'quack-pltish-defn-face "#CC9900")
;; (set-face-attribute 'quack-pltish-defn-face nil :weight 'normal)
;; (set-face-foreground 'quack-pltish-comment-face "#008000")
;; (set-face-foreground 'quack-pltish-selfeval-face "#008080")
;; (set-face-background 'quack-pltish-selfeval-face "#EDF8FF")

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;; customise matching parens
(require 'paren)

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

;; highlight current line
(global-hl-line-mode 1)
(autoload 'highlight-current-line "highlight-current-line")

;; textpad-mode
(require 'textpad)
(textpad-mode)

;; Tabbar
(require 'tabbar)
(tabbar-mode)

;; TODO: bind this to a shortcut, or perhaps a button on the tabbar
(defun tabbar-buffer-groups-switch-mode ()
  "Switches mode between displaying all tabs, or by group"
  (setq tabbar-buffer-groups-function
        (if (eq tabbar-buffer-groups-function 'tabbar-buffer-groups)
          (lambda () (list "All buffers"))
          'tabbar-buffer-groups)))

(setq tabbar-buffer-list-function
      (lambda ()
        (delq nil
              (mapcar #'(lambda (b)
                          (cond
                           ;; Always include the current buffer.
                           ((eq (current-buffer) b) b)
                           ;; remove TAGS buffers
                           ((string-equal "TAGS" (substring (buffer-name b) 0 4)) nil)
                           ;; include buffers for files
                           ((buffer-file-name b) b)
                           ;; dunno what this is ...
                           ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                           ;; or this ...
                           ((buffer-live-p b) b)))
                      (buffer-list)))))

(tabbar-buffer-groups-switch-mode)

;; compatibility with gnome terminal
(global-set-key [(control prior)] 'tabbar-backward)
(global-set-key [(control prior)] 'tabbar-backward)
(global-set-key [(control next)]  'tabbar-forward)

;;; Various key-bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key '[67109053] 'comment-region)
(global-set-key '[67109031] 'uncomment-region)
(global-set-key '[C-return] 'newline-and-indent)
(global-set-key '[(backtab)] 'dabbrev-expand)
(global-set-key [f8] 'other-window)
(global-set-key [f9] 'delete-other-windows)
(global-set-key [f12] 'revert-utf8)

(iswitchb-mode)

;; etags
(require 'etags-select)
(global-set-key "\M-." 'etags-select-find-tag)

;; Add color to a shell running in emacs M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; http://www.emacswiki.org/emacs/EtagsSelect
(defun jds-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
    (defun find-tags-file-r (path)
      "find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
             (possible-tags-file (concat parent "TAGS")))
        (cond
         ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
         ((string= "/TAGS" possible-tags-file) (throw 'found-it nil)) ; we found nothing
         (t (find-tags-file-r (directory-file-name parent))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name))))))

(defun jds-set-tags-file-path ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named 'TAGS'. If found, set 'tags-table-list' with that path as an argument
otherwise raises an error."
  (interactive)
  (let ((tags-file (jds-find-tags-file)))
    (if tags-file
        (setq tags-table-list (list tags-file)))))

;; ;; delay search the TAGS file after open the source file
(add-hook 'emacs-startup-hook
          '(lambda () (jds-set-tags-file-path)))

(require 'w3m-load)

;; Language modes & modifications

(require 'lua-mode)

;; PHP
;(autoload 'php-mode "php-mode" "Major mode for PHP Scripts." t)
(require 'php-mode)

;; Use w3m to browse the php-manual
;(setq php-browse-url 'w3m-browse-url)

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

;; Turn on font lock when in PHP mode
(add-hook 'php-mode-hook
  'turn-on-font-lock)

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

(add-hook 'html-mode-hook
          #'(lambda ()
              (local-set-key '[f6] 'toggle-php-html-mode)))

;; HTML
(require 'autohtml)
(add-hook 'html-mode-hook
          #'(lambda ()
              (local-set-key "\M-+" 'autohtml-close-tag)))

;; Python
(add-hook 'python-mode-hook
      '(lambda ()
         "Turn off Indent Tabs mode."
         (set (make-local-variable 'indent-tabs-mode) nil)
         (set (make-local-variable 'python-indent) 4)))

;; Scheme
(require 'quack)
(setq quack-pretty-lambda-p t)

;; Ruby / Rails
(require 'snippet)
(require 'find-recursive)
(setq load-path (cons "/home/tkn/public/emacs-ruby-mode" load-path))
(require 'ruby-mode)
(add-to-list 'auto-mode-alist
             '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\.rhtml$" . ruby-mode))

;; (setq load-path (cons "/home/tkn/public/emacs-rails" load-path))
;; (require 'rails)
;; (define-key rails-minor-mode-map (kbd "C-c C-RET") 'rails-goto-file-on-current-line)
;; (define-key rails-minor-mode-map [C-return] 'newline-and-indent)

;; (add-hook 'ruby-mode-hook ; is this really rails-mode ?
;; (lambda ()
;; (ruby-electric-mode nil) ;; turn it off, please?
;; (local-set-key [tab] 'indent-selected-region-or-line)
;; ))

;; (add-hook 'after-init-hook
;; (lambda ()
;; (color-theme-charcoal-black)))

(defun jump-to-grep-match ()
  (interactive)
  (when (looking-at "^\\([^:]+\\):\\([0-9]+\\):.*$")
    (let ( (file-name (match-string 1))
           (line-number (string-to-number (match-string 2))))
      (when (not (get-file-buffer file-name))
        (find-file-noselect file-name))
      (if (get-file-buffer file-name)
          (and
           (pop-to-buffer (get-file-buffer file-name))
           (message (concat "goto-line -> " file-name))
           (goto-line line-number))
      ;;; else
        (error (concat "can't pop to file: " file-name " line: " (number-to-string line-number)))
        ))))
;; (local-set-key [return] 'jump-to-grep-match)
