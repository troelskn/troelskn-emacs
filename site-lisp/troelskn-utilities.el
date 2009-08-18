(provide 'troelskn-utilities)

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

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

(defun camelize-buffer-or-region ()
  (interactive)
  (if (y-or-n-p "Select 'y' for methodStyle or 'n' for ClassStyle ")
      (replace-regexp
       "\\b\\([a-z]+_[a-z]+[a-z_]*\\|[a-z]+-[a-z]+[a-z-]*\\)\\b"
       '(replace-eval-replacement replace-quote (camelize-method (match-string 0))))
    (replace-regexp
     "\\b\\([a-z]+_[a-z]+[a-z_]*\\|[a-z]+-[a-z]+[a-z-]*\\)\\b"
     '(replace-eval-replacement replace-quote (camelize (match-string 0))))))

(defun underscore-buffer-or-region ()
  (interactive)
  (replace-regexp
    "\\b\\([a-z]*[A-Z][a-z]+\\)+\\b"
    '(replace-eval-replacement replace-quote (underscore (match-string 0)))))

(defun dasherize-buffer-or-region ()
  (interactive)
  (replace-regexp
    "\\b\\([a-z]*[A-Z][a-z]+\\)+\\b"
    '(replace-eval-replacement replace-quote (dasherize (match-string 0)))))

