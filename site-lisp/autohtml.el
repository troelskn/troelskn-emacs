(require 'sgml-mode)

(provide 'autohtml)

(defvar autohtml-tag-re
  "<\\(/\\)?\\([a-z]+\\)[^/>]*>")

(defun autohtml-find-open-tag ()
  (if (search-backward-regexp autohtml-tag-re nil t)
      (if (match-string 1) ; If it's an end tag
          (if (not (string= (match-string 2) (autohtml-find-open-tag)))
              (error "Unmatched Autohtml tag")
            (autohtml-find-open-tag))
        (match-string 2)) ; Otherwise, return the match
    nil))

(defun autohtml-close-tag ()
  (interactive)
  (let ((open-tag (save-excursion (autohtml-find-open-tag))))
    (if open-tag
        (insert "</" open-tag ">")
      (error "Nothing to close"))))

(global-set-key "\M-+" 'autohtml-close-tag)
;;(global-set-key "\M-=" 'autohtml-close-tag)
;;(define-key html-mode-map (kbd "C-9") 'autohtml-close-tag)

