(require 'org-table)

(defvar substitution-templates nil)

(defun setup-table (beg end)
  (goto-char beg)
  (re-search-forward "file[[:space:]]+['\"]?\\([a-zA-Z0-9-_]+\\)." end)
  (let ((name (match-string 1))
        (orgtbl-gubbins))
    (end-of-line)
    (goto-char (1+ (point)))
    (setq orgtbl-gubbins (concat "#+BEGIN RECEIVE ORGTBL " name
                                 "\n#+END RECEIVE ORGTBL " name
                                 "\n\n#+ORGTBL: SEND " name
                                 " orgtbl-to-substitution :no-escape t\n"))
    (insert orgtbl-gubbins)
    (setq beg (point))
    (setq end (+ end (length orgtbl-gubbins)))
    (re-search-forward "{\\s-*pattern\\s-*{" end t)
    (setq end (- end (length (match-string 0))))
    (replace-match "" t t)
    (while (re-search-forward "{\\|}" (1+ end) t)
      (replace-match "")
      (setq end (- end 1))))
  (goto-char beg)
  (while (search-forward "|" (1+ end) t)
    (replace-match "\\vert" t t)
    (setq end (+ end 4)))
  (list beg end))

;;;###autoload
(defun substitution-table-convert-region (beg0 end0 &optional separator)
  ;; This function is only slightly modified code from org-table
  "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is included.

SEPARATOR specifies the field separator in the lines.  It can have the
following values:

'(4)     Use the comma as a field separator
'(16)    Use a TAB as field separator
integer  When a number, use that many spaces as field separator
nil      When nil, the command tries to be smart and figure out the
         separator in the following way:
         - when each line contains a TAB, assume TAB-separated material
         - when each line contains a comma, assume CSV material
         - else, assume one or more SPACE characters as separator."
  (interactive "rP")
  (let* ((beg (min beg0 end0))
         (end (max beg0 end0))
         (beg-and-end (setup-table beg end))
         re)
    (setq beg (pop beg-and-end))
    (setq end (pop beg-and-end))
    (goto-char beg)
    (beginning-of-line 1)
    (setq beg (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    ;; Get the right field separator
    (unless separator
      (goto-char beg)
      (setq separator
            (cond
             ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
             ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
             (t 1))))
    (goto-char beg)
    (if (equal separator '(4))
        (while (< (point) end)
          ;; parse the csv stuff
          (cond
           ((looking-at "^") (insert "| "))
           ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
           ((looking-at "[^,\n]+") (goto-char (match-end 0)))
           ((looking-at "[ \t]*,") (replace-match " | "))
           (t (beginning-of-line 2))))
      (setq re (cond
                ((equal separator '(4)) "^\\|\"?[ \t]*,[ \t]*\"?")
                ((equal separator '(16)) "^\\|\t")
                ((integerp separator)
                 (if (< separator 1)
                     (error "Number of spaces in separator must be >= 1")
                   (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
                (t (error "This should not happen"))))
      (while (re-search-forward re end t)
        (replace-match "| " t t)))
    (goto-char beg)
    (org-table-align))
  (org-table-begin)
  (org-table-insert-hline)
  (orgtbl-send-table)
  (orgtbl-toggle-comment))

;;;###autoload
(defun substitution-table-convert-file (buffer)
  (interactive "b")
  (set-buffer buffer)
  (goto-char (point-min))
  (let (braces start end)
    (while (re-search-forward "file\\s-*[a-zA-Z0-9-_]+\\.[a-zA-Z]+"
                              (point-max) t)
      (beginning-of-line)
      (setq start (point))
      (search-forward "{")
      (setq braces 1) ; Just passed the first brace so start at 1
      (while (>= braces 1)
        (if (char-equal (char-after) ?{)
            (setq braces (1+ braces))
          (if (char-equal (char-after) ?})
              (setq braces (- braces 1))))
        (forward-char)
        )
      (backward-char)
      (setq end (point))
      (substitution-table-convert-region start (point))
      (goto-char end)
      )))

;;;###autoload
(defun orgtbl-to-substitution (table params)
  "Convert the Orgtbl mode TABLE to substitution file syntax."
  (let* ((params2
          (list
           :no-escape t
           :tend "}"
           :lstart " {"
           :lend "}"
           :sep ", ")))
    ;; Convert \vert to | and put "" in blank cells
    (replace-regexp-in-string
     "\\\\vert" "|"
     (replace-regexp-in-string
      ", }" ", \"\"}"
      (replace-regexp-in-string
       "\\( ,\\)" " \"\","
       (concat "{\npattern"
               (orgtbl-to-generic 
                table (org-combine-plists params2 params)))
       nil nil 1)
      )
     :literal t)))

(defun insert-templates (buffer &optional templates)
  (set-buffer buffer)
  (goto-char (point-min))
  (unless templates
    (setq templates substitution-templates))
  ;; Search for template includes
  (while (re-search-forward
          ;; Look for includes of .template or .db files
          "^include[[:blank:]]+\"\\([_[:word:]]+\\(?:\\.template\\|\\.db\\)\\)\"" nil t)
    ;; Get the template path from alist "templates"
    (let ((template-path (gethash (match-string-no-properties 1) templates)))
      (if template-path
          (progn (insert "\n")
                 (insert-file-contents template-path))))))

(defun read-template-macros (filename);; paths)
  "Read template file and include and return all macros."
  (let ((macros '())
        (templates substitution-templates))
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
          (add-to-list 'macros (match-string-no-properties 1) t))))
    macros))

(defun remove-comments ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#" nil t)
      ;; Move to the end of the last word before #
      (backward-word)
      (forward-word)
      (kill-line))))

(defun expand-macros (macros)
  (maphash (lambda (key value) "Expand macros in value from 'macros' hash"
             ;; Find a macro ie $(MACRO) in value
             (if (string-match "\\$\(\\([A-Z0-9_]*\\)\)" value)
                 ;;(prin1 (match-string 1 value) (get-buffer "*print*"))
                 ;; Look for the macro in the macros hash and replace it in value
                 (puthash key
                          (replace-match (gethash (match-string 1 value) macros) t nil value)
                          macros)))
           macros)
  macros)

(defun find-macros-in-buffer ()
  ;; Search for macros
  (goto-char (point-min))
  (let ((macros (make-hash-table :test 'equal)))
    (while (re-search-forward
            "^\\([A-Z0-9_]*\\)[[:blank:]]*=[[:blank:]]*\\(.*\\)" nil t)
      (puthash (match-string-no-properties 1)
               (match-string-no-properties 2)
               macros))
    macros))

(ert-deftest test-get-templates ()
  "Checks that get-templates returns all of the templates"
  (let ((macros (make-hash-table :test 'equal)))
    (puthash "SUPPORT" "/dls_sw/prod/R3.14.12.3/support" macros)
    (puthash "TPMAC" "/dls_sw/prod/R3.14.12.3/support/tpmac/3-10dls18" macros)
    (should (equal (gethash "pmacController.template" (get-templates macros)) "/dls_sw/prod/R3.14.12.3/support/tpmac/3-10dls18/db/pmacController.template"))))

(defun get-templates (macros)
  "Get the templates in PATH/db where PATH is the value in the hash macros"
  (prin1 macros (get-buffer "*print*"))
  (let ((template-paths (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (let ((db-path (concat value "/db/")))
                 ;; If the db-path exists add all of the templates in it
                 ;; to the template-paths hash
                 (if (file-directory-p db-path)
                     (dolist (template-name (directory-files db-path))
                       (if (string-match "\\.template\\|\\.db" template-name)
                           (puthash template-name (concat db-path template-name) template-paths))))))
             macros)
    template-paths))

(defun get-macros (path)
  "Get macros defined in the file at path"
  (save-current-buffer
    (with-temp-buffer
      (insert-file-contents path)
      (remove-comments)
      (expand-macros (find-macros-in-buffer)))))

(defun substitution-get-template-macros (release-path)
  (interactive "fSelect RELEASE file: ")
  (setq-local substitution-templates (get-templates (get-macros release-path))))

(defun substitution-get-docs-from-template (filename)
  "Look for and return the documentation section of a template"
  (let ((templates substitution-templates)
        (docs (concat "# Template: " (file-name-base filename) "\n"))
        (macros (read-template-macros filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (insert "\n")
      (insert-templates (current-buffer) templates)
      (goto-char (point-min))
      (map 'list (lambda (macro)
                   (save-excursion
                     (if (re-search-forward (concat "# *% *macro, *" macro) nil t)
                         (setq docs
                               (concat docs (buffer-substring-no-properties
                                             (point-at-bol) (point-at-eol)) "\n")))))
           macros)
      (concat docs "\n"))))

;;;###autoload
(defun substitution-table-from-template ()
  (interactive)
  (if (not substitution-templates)
      (call-interactively 'substitution-get-template-macros))
  (let* ((template (completing-read "Select a template: "
                                    substitution-templates))
         (macros (read-template-macros (gethash template
                                                substitution-templates))))
    (insert (substitution-get-docs-from-template
             (gethash template substitution-templates)))
    (insert (concat "file " template
                    "\n#+ BEGIN RECEIVE ORGTBL " template
                    "\n#+ END RECEIVE ORGTBL " template
                    "\n\n#+ORGTBL: SEND " template
                    " orgtbl-to-substitution :no-escape t\n"))
    (org-table-create (concat
                       (number-to-string (cl-list-length macros)) "x2"))
    (forward-char)
    (dolist (heading macros)
      (if (not (equal (compare-strings heading 0 2 "__" nil nil) t))
          (progn (insert heading)
                 (org-table-next-field))
        (org-table-delete-column))))
  (org-table-align))

;;;###autoload
(defun substitution-fill-from-region (beg end)
  "Look for the specified template and fill in any defined variables"
  (interactive "r")
  (substitution-table-convert-region beg end)
  (orgtbl-toggle-comment)
  (goto-char (org-table-begin))
  (save-excursion
    (re-search-backward "file \\([-_a-zA-Z0-9]+\\(?:\\.template\\|\\.db\\)\\)"))
  (message "Filename match: %s" (match-string-no-properties 1))
  (let* ((template-name (match-string-no-properties 1))
         (macros (read-template-macros
                  (gethash template-name substitution-templates)))
         (head-pos 0)
         (last-head-pos 0))
    (message "Macros: %s" (type-of macros))
    (map 'list (lambda (heading)
                 (message "Heading: %s" heading)
                 (save-excursion
                   (unless (setq head-pos
                                (re-search-forward (concat "| *" heading " *|") (point-at-eol) t))
                     (message "last-head-pos: %s" last-head-pos)
                     (goto-char (1+ last-head-pos))
                     (org-table-insert-column)
                     (insert heading))
                   (if head-pos
                       (setq last-head-pos head-pos))))
         macros)))

;;;###autoload
(define-derived-mode epics-substitution-mode fundamental-mode
  (setq comment-start "#")
  (orgtbl-mode)
  (visual-line-mode 0)
  (setq truncate-lines t)
  (setq mode-name "epics-substitution"))

(provide 'epics-substitution)
