(defvar substitution--templates nil)
(defvar substitution--regex-string
  "file[[:space:]]+['\"]?\\(\\$([a-zA-Z0-9-_]+)/db/\\)?\\([a-zA-Z0-9-_]+\\).")

(defun find-char (start end char)
  "looks for characters outside of strings"
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
  "replaces characters given a list of positions"
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
  "formats table layour to keep a constant look"
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
  "aligns table based on comma separator"
  (interactive "r")
  (save-excursion
   (save-restriction
     (narrow-to-region beg end)
      (goto-char (point-min))
      (replace-chars (point-min) (point-max) "|" ?,)
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)|" 1 1 1)
      (replace-chars (point-min) (point-max)"," ?|)) ))

(defun getbounds ()
  "finds bounds of substitution table"
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
  "align table given point is in substitution table starting line"
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


;;do a thing to count the number of commas in the headings
;;this will allow to move table to next line when 
;;do a thing to align the table with a shortcut from anywhere in the table by going up
;;a sexp expression C-M-u (end of line as well)




;;new stuff
(defun substitution-get-template-macros (release-path)
  (interactive "fSelect RELEASE file: ")
  (setq-local substitution--templates
              (get-templates (with-temp-buffer
                               (insert-file-contents release-path)
                               (get-macros))))
  (hash-table-values substitution--templates))

(defun read-template-macros (filename);; paths)
  "Read template file and include and return all macros."
  (message "got here")
  (let ((macros '())
        (templates substitution--templates))
    (save-current-buffer
      (with-temp-buffer
        (print (concat "filename " filename) (get-buffer "output2"))
        (insert-file-contents filename)
        (insert-templates (current-buffer) templates)
        (goto-char (point-min))
        (while (re-search-forward
                "#[[:blank:]]*%[[:blank:]]*macro,[[:blank:]]*\\([_[:word:]]+\\)"
                nil t)
          (add-to-list 'macros (match-string-no-properties 1) t))
        (goto-char (point-min))
        (while (re-search-forward "$(\\([A-Za-z_]*\\))" nil t)
          (add-to-list 'macros (match-string-no-properties 1) t)))
    macros)))

(defun substitution-get-docs-from-template (filename)
  "Look for and return the documentation section of a template"
  (let ((templates substitution--templates)
        (docs (concat "# Template: " (file-name-base filename) "\n"))
        (macros (read-template-macros filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (insert "\n")
      (insert-templates (current-buffer) templates)
      (goto-char (point-min))
      (mapc (lambda (macro)
              (save-excursion
                (if (re-search-forward (concat "# *% *macro, *" macro) nil t)
                    (setq docs
                          (concat docs (buffer-substring-no-properties
                                        (point-at-bol) (point-at-eol)) "\n")))))
            macros)
      (concat docs "\n"))))


;; (defun insert-template (macros)
;;   (dolist (heading macros)
;;     (if (not (equal (compare-strings heading 0 2 "__" nil nil) t))
;;         (progn (insert heading)
               
;;     )
;;   )

(defun substitution-table-from-template ()
  (interactive)
  (if (not substitution--templates)
      (call-interactively 'substitution-get-template-macros))
  (let* ((template (completing-read "Select a template: "
                                    substitution--templates))
         (macros (read-template-macros (gethash template
                                                substitution--templates))))
    (insert (substitution-get-docs-from-template
             (gethash template substitution--templates)))
    (insert (concat "file " template
                    "\n{"
                    "\npattern"
                    "\n\n"))
    ;(insert-template)
    (insert "}")))


  

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
