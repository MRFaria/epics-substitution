(provide 'substitution-table-convert-region)
(provide 'substitution-table-convert-file)
(provide 'substitution-table-from-template)
(provide 'orgtbl-to-substitution)

(require 'org-table)

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
         re)
    (setq beg-and-end (setup-table beg end))
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
       "\\(,\\)\\( \\)\\(,\\)" " \"\""
       (replace-regexp-in-string
        "\\(,\\)\\( \\)\\(,\\)" " \"\""
        (concat "{\npattern"
                (orgtbl-to-generic
                 table (org-combine-plists params2 params)))
        nil nil 2)
       nil nil 2)
      )
     :literal t)))

(defun insert-templates (paths buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  (while (re-search-forward
          "^include[[:blank:]]+\"\\([_[:word:]]+.template\\)\"" nil t)
    (let ((index 0))
      ;; Check if file exists and insert if it does otherwise increment
      (while (cond
              ((file-readable-p
                (concat (nth index paths) "/db/"
                        (match-string-no-properties 1)))
               (insert-file-contents (concat (nth index paths) "/db/"
                                             (match-string-no-properties 1)))
               nil)
              ((incf index)
               (nth index paths)))))))

(defun read-template-macros (filename paths)
  "Read template file and includes and return all macros."
  (let '(macros '())
    (save-current-buffer
      (with-temp-buffer
        (insert-file-contents filename)
        (insert-templates paths (current-buffer))
        (goto-char (point-min))
        (while (re-search-forward
                "#[[:blank:]]*%[[:blank:]]*macro,[[:blank:]]*\\([[:word:]]+\\)"
                nil t)
          (add-to-list 'macros (match-string-no-properties 1)))
        (print macros (get-buffer "output"))))
    macros))

(defun get-paths-from-release (path)
  (let '(paths '())
    (setq macros (make-hash-table :test 'equal))
    (save-current-buffer
      (with-temp-buffer
        (insert-file-contents path)
        ;; Scan through the file and kill all comments
        (goto-char (point-min))
        (while (search-forward "#" nil t)
          (kill-line))
        ;; Search for macros
        (goto-char (point-min))
        (while (re-search-forward
                "^\\([A-Z_]*\\)[[:blank:]]*=[[:blank:]]*\\(.*\\)" nil t)
          (puthash (match-string-no-properties 1)
                   (match-string-no-properties 2)
                   macros)
          )
        ;; Grab paths and substitute any macros
        (goto-char (point-min))
        (while (re-search-forward "\$\(\\([A-Z_]*\\)\)\\(.*\\)" nil t)
          (if (gethash (match-string-no-properties 1) macros)
              (add-to-list 'paths (concat
                                   (gethash (match-string-no-properties 1)
                                            macros)
                                   (match-string-no-properties 2)))))))
    paths))

(defun substitution-table-from-template (template-file)
  "Insert an org-table with headings from a template containing #%macros"
  (interactive "f")
  (setq paths (get-paths-from-release (concat
                                       (file-name-directory template-file)
                                       "../../configure/RELEASE")))
  (setq macros (read-template-macros template-file paths))
  (setq name (file-name-base template-file))
  (insert (concat "file " (file-name-nondirectory template-file)
                  "\n#+ BEGIN RECEIVE ORGTBL " name
                  "\n#+ END RECEIVE ORGTBL " name
                  "\n\n#+ORGTBL: SEND " name
                  " orgtbl-to-substitution :no-escape t\n"))
  (org-table-create (concat
                     (number-to-string (list-length macros)) "x2"))
  (forward-char)
  (dolist (heading (nreverse macros))
    (insert heading)
    (org-table-next-field))
  (org-table-align))

