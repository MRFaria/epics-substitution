(defvar substitution--templates nil)
(defvar substitution--regex-string
  "file[[:space:]]+['\"]?\\(\\$([a-zA-Z0-9-_]+)/db/\\)?\\([a-zA-Z0-9-_]+\\).")

(defun find-char (start end char)
  (interactive "r")
  (print "test")
  (goto-char start)
  (let (matches)
    (while (< (point) end)
      (if (equal (char-after) char)
          (if (nth 3 (syntax-ppss (point)))
              (print "in string")
            (push (point) matches)))
      (forward-char))
    (nreverse matches)))

(defun replace-chars (beg end sep char)
  (interactive "r")
  (let ((char-list (find-char beg end char)))
    (print char-list)
    (dolist (point char-list)
      (goto-char point)
      (delete-char 1)
      (insert sep))))


(defun just-one-space-in-region (start end)
  "replace all whitespace before and after separator"
  (interactive "r")
  (goto-char start)
  (catch 'exit-function
    (while (< (point) end)
      (if (equal (char-after) ?,)
          (if (nth 3 (syntax-ppss (point)))
              (print "in string")
            (just-one-space) (forward-char) (just-one-space) (forward-char -2)))
      (condition-case nil
          (forward-char)
        (error (throw 'exit-function t))))))

(defun format-table (beg end)
  (interactive "r")
  (goto-char beg)
  (cond ((search-forward-regexp "pattern[ ]?+{" end t )
           (forward-char -1)
           (insert-char ?\n)
           (insert-tab))
          
          ((search-forward-regexp "pattern[ ]?+\n[ ]?+{" end t)
           '(nil))
          ((t)
           (print "bad-table"))))
  
(defun align-table (beg end)
  (interactive "r")
  (save-excursion
   (save-restriction
     (narrow-to-region beg end)
      (goto-char (point-min))
      (replace-chars (point-min) (point-max) "|" ?,)
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)|" 1 1 1)
      (replace-chars (point-min) (point-max)"," ?|)) ))

(defun getbounds ()
  (interactive)
  (beginning-of-line)
  (let ((start nil)
        (end nil))
    (search-forward ".template" (line-end-position))
    (setq start (point))
    (forward-sexp)
    (setq end (point))
    (cl-values start end)))

(defun substitution-align-table ()
  (interactive)
  (let ((bounds nil)
        (beg nil)
        (end nil))
    (setq bounds (getbounds))
    (setq beg (car bounds))
    (setq end (car (cdr bounds)))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (format-table (point-min) (point-max))
        (goto-char (point-min))
        (search-forward "pattern" (point-max))
        (align-regexp (point) (point-max) "\\(\\s-*\\){" 1 4 1)
        (print "3")
        (just-one-space-in-region (point-min) (point-max))
        (print "4")
        (align-table (point-min) (point-max))
        ))))


  

(defvar epics-substitution-mode-syntax-table nil)
(defvar epics-substitution-mode-highlights nil)
(setq epics-substitution-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; bash style comment: “# …”
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))
(setq epics-substitution-mode-highlights
      '(("file\\|pattern" . font-lock-function-name-face)))

;;;###autoload
(define-derived-mode epics-substitution-mode-csv fundamental-mode "epics-substitution"
  (setq comment-start "#")
  (setq font-lock-defaults '(epics-substitution-mode-highlights))
  (visual-line-mode 0)
  (setq truncate-lines t)
  (local-set-key (kbd "C-c #") 'orgtbl-toggle-comment))

(provide 'epics-substitution)
