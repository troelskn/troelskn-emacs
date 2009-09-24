(provide 'troelskn-utilities)

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun replace-regexp-in-buffer-or-region (regexp to-string)
  "Like `REPLACE-REGEXP`, but will act upon the region, if selected."
  (if mark-active
      (let
          ((selection-text (buffer-substring-no-properties (region-beginning) (region-end)))
           (begin (region-beginning))
           (end (region-end)))
        (save-excursion
          (let (
                (new-string (save-excursion
                              (with-temp-buffer
                                (insert selection-text)
                                (goto-char 0)
                                (replace-regexp regexp to-string)
                                (buffer-string)))))
            (goto-char begin)
            (delete-region begin end)
            (insert new-string))
        ))
    (replace-regexp regexp to-string)))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
      (mapconcat 'identity (mapcar-head
                            '(lambda (word) (downcase word))
                            '(lambda (word) (capitalize (downcase word)))
                            (split-string s "_")) ""))

(defun underscore (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is an underscore \"_\".

    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun dasherize (s &optional start)
  (underscore s "-" start))

(defun camelize-buffer-or-region-method ()
  (interactive)
  (replace-regexp-in-buffer-or-region
   "\\b\\([a-z]+_[a-z]+[a-z_]*\\|[a-z]+-[a-z]+[a-z-]*\\)\\b"
   '(replace-eval-replacement replace-quote (camelize-method (match-string 0)))))

(defun camelize-buffer-or-region-class ()
  (interactive)
  (replace-regexp-in-buffer-or-region
   "\\b\\([a-z]+_[a-z]+[a-z_]*\\|[a-z]+-[a-z]+[a-z-]*\\)\\b"
   '(replace-eval-replacement replace-quote (camelize (match-string 0)))))

(defun camelize-buffer-or-region ()
  (interactive)
  (if (y-or-n-p "Select 'y' for methodStyle or 'n' for ClassStyle ")
      (camelize-buffer-or-region-method)
    (camelize-buffer-or-region-class)))

(defun underscore-buffer-or-region ()
  (interactive)
  (replace-regexp-in-buffer-or-region
    "\\b\\([a-z]*[A-Z][a-z]+\\)+\\b"
    '(replace-eval-replacement replace-quote (underscore (match-string 0)))))

(defun dasherize-buffer-or-region ()
  (interactive)
  (replace-regexp-in-buffer-or-region
    "\\b\\([a-z]*[A-Z][a-z]+\\)+\\b"
    '(replace-eval-replacement replace-quote (dasherize (match-string 0)))))

(defun shift-region-right (arg)
  (interactive "*p")
  (indent-rigidly (region-beginning) (region-end) 1))

(defun shift-region-left (arg)
  (interactive "*p")
  (indent-rigidly (region-beginning) (region-end) -1))

