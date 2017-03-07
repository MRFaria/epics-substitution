# epics-substitution

## Installation
Copy the epics-substitution elisp files into your .emacs.d directory, which should be created when emacs is first run.

epics-substitution requires a recent version of org-mode, the API between the version of org-mode that comes pre-installed with emacs is incompatable with more recent versions of org-mode, and SOME functions of epics-substitution-mode don't work with the emacs stock org-mode version. As far as I know, only substitution-convert-table (used to convert between epics-substitution-tables and org-tables) fails.

In your ~/.emacs.d/init.el file add the following:

1) To load the lisp files automatically when needed:
```elisp
(autoload 'epics-substitution-mode "epics-substitution.el" "" t)
(autoload 'epics-template-mode "epics-template.el" "" t)
```
2) To set the relevant modes automatically by file extension:
```elisp
(add-to-list 'auto-mode-alist '("\\.substitutions\\'" . epics-substitution-mode))
(add-to-list 'auto-mode-alist '("\\.\\(db\\|template\\)\\'" . epics-template-mode))
```

## Usage
Start typing: ```M-x substitution-``` followed by tab to list the possible functions provided by epics-substitution mode


1) To align an existing table, run the following on the first line of the table (file .....):
```elisp
M-x substitution-align-table
```

2) To open a template for reading run:
```elisp
M-x substitution-open-template
```
which prompts for a configure/RELEASE file and displays a list
of available templates to open


3) To insert a template into the current substitutions file:
```elisp
M-x substitution-table-from-template
```
This creates a skeleton org-table from macros found in the template file selected.
Fill in this table and run ```substitution-convert-table``` (C-c C-f) to convert it into
an epics-subtitution-table




The following keybindings are set by epics-substitution-mode:
```elisp
   (local-set-key (kbd "C-c C-f") 'substitution-convert-table)
   (local-set-key (kbd "M-n") 'scroll-template-down)
   (local-set-key (kbd "M-p") 'scroll-template-up))
```


