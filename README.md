# typespec-ts-mode

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/typespec-ts-mode-badge.svg)](https://melpa.org/#/typespec-ts-mode)

Emacs major mode for TypeSpec (using tree-sitter).

## Installing

typespec-ts-mode is [available on MELPA](https://melpa.org/#/typespec-ts-mode). Emacs 29.1 or above with tree-sitter support is required.

You'll also need the tree-sitter grammar [here](https://github.com/happenslol/tree-sitter-typespec/blob/main/grammar.js) to be installed. Add it to `treesit-language-source-alist`:

```elisp
(add-to-list
 'treesit-language-source-alist
 '(typespec "https://github.com/happenslol/tree-sitter-typespec"))
```

Then run `M-x treesit-install-language-grammar [RET] typespec` to install it.
