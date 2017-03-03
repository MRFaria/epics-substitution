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
        (let
            ((transform (intern (match-string 1 format)))
             (params (and (match-end 2)
                          (read (concat "(" (match-string 2 format) ")"))))
             (table (org-table-to-lisp
                     (buffer-substring-no-properties
                      (org-table-begin) (org-table-end)))))

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

(defun find-char (start end char)
  "looks for characters outside of strings"
  (interactive "r")
  (goto-char start)
  (let (matches)
    (while (< (point) end)
      (if (equal (char-after) char)
          (if (nth 3 (syntax-ppss (point)))
              (message "in-string")
            (push (point) matches)))
      (forward-char))
    (nreverse matches)))

(defun replace-chars (beg end sep char)
  "replaces characters given a list of positions"
  (let ((char-list (find-char beg end char)))
    (dolist (point char-list)
      (goto-char point)
      (delete-char 1)
      (insert sep))))

(defun just-one-space-in-region (start end)
  "replace all whitespace before and after separator"
  (goto-char start)
  (catch 'exit-function
    (while (< (point) end)
      (if (equal (char-after) ?,)
          (if (nth 3 (syntax-ppss (point)))
            (just-one-space) (forward-char) (just-one-space) (forward-char -2)))
      (condition-case nil
          (forward-char)
        (error (throw 'exit-function t))))))

(defun format-table (beg end)
  "formats table layour to keep a constant look"
  (goto-char beg)
  (cond ((search-forward-regexp "pattern[ ]?+{" end t )
           (forward-char -1)
           (insert-char ?\n)
           (insert-tab))

          ((search-forward-regexp "pattern[ ]?+\n[ ]?+{" end t)
           '(nil))
          (t
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
            (goto-char (point-min))
            ;escape pipes that already exist in the table
            (while (re-search-forward "|" (point-max) t)
              (replace-match "\\\\vert"))
            ;convert commas to pipes
            (replace-chars (point-min) (point-max) "|" ?,)
            (goto-char (point-min))
            (while (re-search-forward "{\\|}" (point-max) t)
              (replace-match "|"))))))

    (goto-char beg-table)))
;        (replace-chars (point-min) (point-max) "}" ?|)
 ;       (replace-chars (point-min) (point-max) "," ?|)))))

(defun align-table (beg end)
  "aligns the substitution table
  This uses the ¦ character to align the table, as sometimes there are commas in the table
  fields"
  (interactive "r")
  (save-excursion
   (save-restriction
     (narrow-to-region beg end)
     (goto-char (point-min))
     (replace-chars (point-min) (point-max) "¦" ?,)
     (align-regexp (point-min) (point-max) "\\(\\s-*\\)¦" 1 1 1)
     (replace-chars (point-min) (point-max)"," ?¦))))


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

;;;###autoload
(defun orgtbl-to-substitution (table params)
  "Convert the Orgtbl mode TABLE to substitution file syntax."
  (let* ((params2
          (list
           ;:no-escape t
           :lstart " { "
           :lend " }"
           :sep ", ")))
    ;; Convert \vert to | and put "" in blank cells
    (replace-regexp-in-string
     "\\\\vert" "|"
     (replace-regexp-in-string
      ", }" ", \"\"}"
      ;; (replace-regexp-in-string
      ;;  "\\( ,\\)" " \"\","
       (concat
               (orgtbl-to-generic
                table (org-combine-plists params2 params)))
       nil nil 1)
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
               (let ((db-path (if (string= value default-directory)
                                  default-directory
                                (concat value "/db/"))))
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
                               (let ((macros (get-macros)))
                                 (puthash "THISIOC" default-directory macros)
                                 macros))))
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

(defun substitution-convert-table ()
  (interactive)
  (if (org-at-table-p)
      (org-table-convert-on-the-spot)
    (convert-substitution-to-table)))

(defun substitution-align-file ()
  (interactive)
  "align tables throughout the file"
  (goto-char (point-min))
  (while (< (point) (point-max))
    (search-forward-regexp "\\.template\\|\\.db" (point-max) t )
    (if (or (nth 4 (syntax-ppss)) (looking-at "#"))
        'nil
      (substitution-align-table))))

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

(defun scroll-template-up ()
  (interactive)
  (search-backward "file" (point-min) t)
  (move-beginning-of-line nil))

(defun scroll-template-down ()
  (interactive)
  ;; check if we are in a comment
  (while (or (nth 4 (syntax-ppss)) (looking-at "#"))
      (forward-char))
  (if (looking-at "file")
      (end-of-line nil))
  (search-forward "file" (point-max) t)
  (move-beginning-of-line nil))


;;;###autoload
(define-derived-mode epics-substitution-mode fundamental-mode "epics-substitution"
  (setq comment-start "#")
  (setq font-lock-defaults '(epics-substitution-mode-highlights))
  (orgtbl-mode 1)
  (local-set-key (kbd "C-c #") 'orgtbl-toggle-comment)
  (local-set-key (kbd "C-c C-f") 'substitution-convert-table)
  (local-set-key (kbd "M-n") 'scroll-template-down)
  (local-set-key (kbd "M-p") 'scroll-template-up))

(provide 'epics-substitution)
