# epics-substitution

## Installation
Copy the epics-substitution elisp files into your .emacs.d directory, which should be created when emacs is first run.

epics-substitution depends on org-mode which is part of recent distributions of GNU Emacs

In your ~/.emacs.d/init.el file add the following:

1) To load the files automatically when needed
```elisp
(autoload 'epics-substitution-mode "epics-substitution.el" "" t)
(autoload 'epics-template-mode "epics-template.el" "" t)
```
2) To set the relevant modes by type
```elisp
(add-to-list 'auto-mode-alist '("\\.substitutions\\'" . epics-substitution-mode))
(add-to-list 'auto-mode-alist '("\\.\\(db\\|template\\)\\'" . epics-template-mode))
```

## Usage
```elisp
M-x substitution-
```
tab completes to the possible functions provided by epics-substitution mode

```elisp
M-x template-
```
does the equivalent for epics-template mode

```elisp
substitution-open-template
```
prompts for a configure/RELEASE file and displays a list
of available templates to open

```elisp
substitution-fill-table-from-region
```
Creates an aligned org table matching the template substitution table.
This can be modified and the substitution table updated with the C-c C-c keybinding. The substitution table will only be updated if the org table is uncommented, which can be done by running C-c-# anywhere in the table.
