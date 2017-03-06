# epics-substitution

## Installation
Copy the epics-substitution elisp files into your .emacs.d directory, which should be created when emacs is first run.

epics-substitution requires a recent version of org-mode, the API between the version of org-mode that comes pre-installed with emacs is incompatable with more recent versions of org-mode, and some functions of substitution-mode don't work with the emacs stock org-mode version.

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
```elisp
M-x substitution-
```
tab completes to the possible functions provided by epics-substitution mode


To align an existing table run:
```elisp
substitution-align-table
```
in the first line of the table

```elisp
substitution-open-template
```
prompts for a configure/RELEASE file and displays a list
of available templates to open

The following keybindings are set by epics-substitution-mode:
```elisp
   (local-set-key (kbd "C-c C-f") 'substitution-convert-table)
   (local-set-key (kbd "M-n") 'scroll-template-down)
   (local-set-key (kbd "M-p") 'scroll-template-up))
```


