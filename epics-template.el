(require 'org-table)

;; (defun epics-template-mode-syntax-table (table &rest list)
;;   "Copy TABLE and set syntax for successive CHARs according to strings S."
;;   (setq table (copy-syntax-table table))
;;   (while list
;;     (modify-syntax-entry (pop list) (pop list) table))
;;   table)

;; (defvar epics-template-mode-syntax-table
;;   (epics-template-mode-syntax-table ()
;; 	?\# "< b"
;; 	?\n "># b"

;; 	?\' "\"'"
;; 	?\` "\"`"
;; 	;; ?$ might also have a ". p" syntax. Both "'" and ". p" seem
;; 	;; to work fine. This is needed so that dabbrev-expand
;; 	;; $VARNAME works.
;; 	?$ "'"
;; 	?! "_"
;; 	?% "_"
;; 	?: "_"
;; 	?. "_"
;; 	?^ "_"
;; 	?~ "_"
;; 	?, "_"
;; 	?= "."
;; 	?\; "."
;; 	?| "."
;; 	?& "."
;; 	?< "."
;; 	?> ".")
;;   "The syntax table to use for Shell-Script mode.
;; This is buffer-local in every such buffer.")

(defvar epics-template-mode-syntax-table nil)
(defvar my-highlights nil)

(setq epics-template-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; bash style comment: “# …”
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        (modify-syntax-entry ?$ "'" synTable)

        synTable))
;; (setq my-highlights
;;       '(("record" . font-lock-function-name-face)
;;         ("field" . font-lock-keyword-face)))
(setq my-highlights
      '(
        ;;horrible to look at but matches $() or ${} macro style
        ("\\(\\$([a-zA-Z0-9.=_-]+)\\)\\|\\(\\${[a-zA-Z0-9.=_-]+}\\)" 0 font-lock-variable-name-face t)
        ("field" . font-lock-keyword-face)
        ("record" . font-lock-function-name-face)
        ))



;;;###autoload
(define-derived-mode epics-template-mode fundamental-mode "template"
  (setq font-lock-defaults '(my-highlights))
  (visual-line-mode 0)
  (setq truncate-lines t))

;; (font-lock-add-keywords 'epics-template-mode
;;                          '("name" 0 font-lock-keyword-face t))


(provide 'epics-template)


