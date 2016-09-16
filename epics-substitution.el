;; Copyright 2016 Mauro Faria
;; Copyright 2016 Adam Bark
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'org-table)

(defvar substitution--templates nil)
(defvar substitution--regex-string
  "file[[:space:]]+['\"]?\\(\\$([a-zA-Z0-9-_]+)/db/\\)?\\([a-zA-Z0-9-_]+\\).")

(defun org-table-convert-on-the-spot (&optional format)
;  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align) ;; make sure we have everything we need
  (let* ((beg (org-table-begin))
         (end (org-table-end))
         (txt (buffer-substring-no-properties beg end))
         (formats '("orgtbl-to-substitution"))
         (format (or format
                     (org-entry-get beg "TABLE_EXPORT_FORMAT" t)))
         buf deffmt-readable)
    (unless format
      (setq deffmt-readable
            org-table-export-default-format)
      (while (string-match "\t" deffmt-readable)
        (setq deffmt-readable (replace-match "\\t" t t deffmt-readable)))
      (while (string-match "\n" deffmt-readable)
        (setq deffmt-readable (replace-match "\\n" t t deffmt-readable)))
      (setq format "orgtbl-to-substitution"));(org-completing-read "Format: " formats nil nil deffmt-readable)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
        (let* ((transform (intern (match-string 1 format)))
               (params (if (match-end 2)
                           (read (concat "(" (match-string 2 format) ")"))))
               (skip (plist-get params :skip))
               (skipcols (plist-get params :skipcols))
               (lines (nthcdr (or skip 0) (org-split-string txt "[ \t]*\n[ \t]*")))
               (lines (org-table-clean-before-export lines))
               (i0 (if org-table-clean-did-remove-column 2 1))
               (table (mapcar
                       (lambda (x)
                         (if (string-match org-table-hline-regexp x)
                             'hline
                           (org-remove-by-index
                            (org-split-string (org-trim x) "\\s-*|\\s-*")
                            skipcols i0)))
                       lines))
               (fun (if (= i0 2) 'cdr 'identity))
               (org-table-last-alignment
                (org-remove-by-index (funcall fun org-table-last-alignment)
                                     skipcols i0))
               (org-table-last-column-widths
                (org-remove-by-index (funcall fun org-table-last-column-widths)
                                     skipcols i0)))

          (unless (fboundp transform)
            (user-error "No such transformation function %s" transform))
          (setq txt (funcall transform table params))

          (delete-region beg end)
          (let ((beg (point))
                (end nil))
            (insert txt "\n")
            (setq end (point))
            (save-excursion
              (save-restriction
                ;this code is repeated, fix
                (narrow-to-region beg end)
                (align-regexp beg (point-max) "\\(\\s-*\\){" 1 3 1)
                (just-one-space-in-region (point-min) (point-max))
                (align-table (point-min) (point-max))))
            (goto-char beg))

          (message "Export done."))
      (user-error "TABLE_EXPORT_FORMAT invalid"))))



(defun find-commas (start end)
   (save-excursion
    (goto-char start)
    (let (matches)
      (while (< (point) end)
        (cond ((= (char-after) ?,)
               (push (point) matches)
               (forward-char))
              ((looking-at "[]\\[{}()]")
               (forward-char))
              (t
               (forward-sexp))))
      (nreverse matches))))

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

(defun convert-substitution-to-table ()
  (interactive)
  (move-end-of-line 0)
  (backward-up-list)
  (let ((beg (point))
        (end (forward-list))
        (beg-table nil))
    (print end)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (re-search-forward "pattern" (point-max) t)
        (forward-line 1)
        (setq beg-table (point))
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (- (point-max) 1))
            (replace-chars (point-min) (point-max) "|" ?,)
            (goto-char (point-min))
            (while (re-search-forward "{\\|}" (point-max) t)
              (replace-match "|"))
            (replace-chars (point-min) (point-max) "|" ?,)))))
    (goto-char beg-table)))
;        (replace-chars (point-min) (point-max) "}" ?|)
 ;       (replace-chars (point-min) (point-max) "," ?|)))))

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


; make this convert the whole file 
(defun getbounds ()
  "finds bounds of substitution table"
  (interactive)
  (beginning-of-line)
  (let ((start nil)
        (end nil))
    (search-forward-regexp "\\.template\\|\\.db" (line-end-position))
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
        (align-regexp (point) (point-max) "\\(\\s-*\\){" 1 3 1)
        (just-one-space-in-region (point-min) (point-max))
        (align-table (point-min) (point-max))
        ))))
;; (defun find-commas (start end)
;;   (save-excursion
;;     (goto-char start)
;;     (let (matches)
;;       (while (< (point) end)
;;         (cond ((= (char-after) ?,)
;;                (push (point) matches)
;;                (forward-char))
;;               ((looking-at "[]\\[{}()]")
;;                (forward-char))
;;               (t
;;                (forward-sexp))))
;;       (nreverse matches))))

;;;###autoload
(defun substitution-table-convert-region (beg0 end0 &optional separator)
   ;; This function is only slightly modified code from org-table
   "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved slightly, 
to make sure a beginning of line in the first line is included.

SEPARATOR specifies the field separator in the lines.  It can have the following values:

'(4)     Use the comma as a field separator
'(16)    Use a TAB as field separator
integer  When a number, use that many spaces as field separator
nil      When nil, the command tries to be smart and figure out the
          separator in the following way:
          - when each line contains a TAB, assume TAB-separated material
          - when each line contains a comma, assume CSV material
          - else, assume one or more SPACE characters as separator."
   (interactive "rP")
   (unwind-protect
       (let ((debug-on-error t) (comma-list nil))
         (with-demoted-errors "Error: %S"
             ;; fundamental mode avoids issue
             ;; with subtitution-mode-comments
             (fundamental-mode)
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
               (setq separator '(4)))
             (goto-char beg)
             ;; Replace commas outside quotes
             ;; with pipess
             (setq comma-list (find-commas beg end))
             (dolist (comma comma-list)
               (goto-char comma)
               (delete-char 1)
               (insert "|"))
             (goto-char beg)
             (if (equal separator '(4))
                 (while (< (point) end)
                   (cond
                    ((looking-at "^") (insert "| "))
                    ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
                    ((looking-at "[^,\n]+") (goto-char (match-end 0)))
                    ;; moves down one line
                    (t (beginning-of-line 2)))))
             (goto-char beg)
             (org-table-align))
           (org-table-begin)
           (org-table-insert-hline)
           (orgtbl-send-table)
           (orgtbl-toggle-comment)))
         
         ;; This always runs, even when errors occur above
         (epics-substitution-mode)))


(defun setup-table (beg end)
  (goto-char beg)
  (re-search-forward substitution--regex-string end)
  (let ((name (match-string 2))
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
(defun substitution-table-convert-file (buffer)
  (interactive "b")
  (set-buffer buffer)
  (goto-char (point-min))
  (let (braces start end)
    (while (re-search-forward substitution--regex-string (point-max) t)
      (if (org-in-commented-line) () 
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
        (goto-char end)))))

;;;###autoload
(defun orgtbl-to-substitution (table params)
  "Convert the Orgtbl mode TABLE to substitution file syntax."
  (let* ((params2
          (list
           :no-escape t
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
       (concat 
               (orgtbl-to-generic 
                table (org-combine-plists params2 params)))
       nil nil 1))
     :literal t)))

;;;###autoload
(defun orgtbl-to-substitution-old (table params)
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
       (concat "{\npattern\n"
               (orgtbl-to-generic 
                table (org-combine-plists params2 params)))
       nil nil 1))
     :literal t)))

(defun insert-templates (buffer &optional templates)
  (set-buffer buffer)
  (goto-char (point-min))
  (unless templates
    (setq templates substitution--templates))
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

(defun get-macros ()
  "Get macros defined in current buffer"
  (remove-comments)
  (expand-macros (find-macros-in-buffer)))

(defun substitution-get-template-macros (release-path)
  (interactive "fSelect RELEASE file: ")
  (setq-local substitution--templates
              (get-templates (with-temp-buffer
                               (insert-file-contents release-path)
                               (get-macros))))
  (hash-table-values substitution--templates))

(defun substitution-open-template ()
  (interactive)
  (if (not substitution--templates)
      (call-interactively 'substitution-get-template-macros))
  (let* ((template (completing-read "Select a template: "
                                    substitution--templates))
         (filename (gethash template substitution--templates)))
         (switch-to-buffer (find-file-noselect filename))))



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

;;;###autoload
(defun substitution-table-from-template-old ()
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
                    "\n#+ BEGIN RECEIVE ORGTBL " template
                    "\n#+ END RECEIVE ORGTBL " template
                    "\n\n#+ORGTBL: SEND " template
                    " orgtbl-to-substitution-old :no-escape t\n"))
    (org-table-create (concat
                       (number-to-string (length macros)) "x2"))
    (forward-char)
    (dolist (heading macros)
      (if (not (equal (compare-strings heading 0 2 "__" nil nil) t))
          (progn (insert heading)
                 (org-table-next-field))
        (org-table-delete-column))))
  (org-table-align)
  (org-table-delete-column))

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
                    "\n{\n"
                    "pattern\n"
                    "}"
                    ))
    (beginning-of-line)
    (org-table-create (concat
                       (number-to-string (length macros)) "x2"))
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
    (re-search-backward
     "file \\([-_a-zA-Z0-9]+\\(?:\\.template\\|\\.db\\)\\)"))
  (message "Filename match: %s" (match-string-no-properties 1))
  (let* ((template-name (match-string-no-properties 1))
         (macros (read-template-macros
                  (gethash template-name substitution--templates)))
         (head-pos 0)
         (last-head-pos 0))
    (message "Macros: %s" (type-of macros))
    (mapc (lambda (heading)
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
          'macros)))


(defun substitution-convert-table ()
  (interactive)
  (if (org-at-table-p)
      (org-table-convert-on-the-spot)
    (convert-substitution-to-table)))

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
(define-derived-mode epics-substitution-mode fundamental-mode "epics-substitution"
  (setq comment-start "#")
  (setq font-lock-defaults '(epics-substitution-mode-highlights))
  (orgtbl-mode 1)
  (visual-line-mode 0)
  (setq truncate-lines t)
  (local-set-key (kbd "C-c #") 'orgtbl-toggle-comment)
  (global-set-key (kbd "C-c C-f") 'substitution-convert-table))

(provide 'epics-substitution)
