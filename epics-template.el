;; Copyright 2016 Mauro Faria
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


(defvar epics-template-mode-syntax-table nil)
(defvar my-highlights nil)

(setq epics-template-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; bash style comment: “# …”
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        (modify-syntax-entry ?$ "'" synTable)
        synTable))

(setq my-highlights
      '(
        ;;horrible to look at, but matches $() or ${} style macros, might separate if it gets worse
        ("\\(\\$([a-zA-Z0-9.=_-]+)\\)\\|\\(\\${[a-zA-Z0-9.=_-]+}\\)" 0 font-lock-variable-name-face t)
        ("field" . font-lock-keyword-face)
        ("record" . font-lock-function-name-face)
        ))

;;;###autoload
(define-derived-mode epics-template-mode fundamental-mode "template"
  (setq font-lock-defaults '(my-highlights))
  (visual-line-mode 0)
  (setq truncate-lines t))

(provide 'epics-template)


