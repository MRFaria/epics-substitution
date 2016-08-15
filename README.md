# epics-substitution-mode

## Installation
Copy the epics-substitution folder into your .emacs.d directory
Depends on org-mode which is part of recent distributions of GNU Emacs

Add the files to be automatically loaded
```elisp
(autoload 'epics-substitution-mode "epics-substitution.el" "" t)
(autoload 'epics-template-mode "epics-template.el" "" t)
```
Add some hooks to set the modes depending on file typ
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


substitution-open-template prompts for a confifure/RELEASE file and displays a list
of available templates to open

