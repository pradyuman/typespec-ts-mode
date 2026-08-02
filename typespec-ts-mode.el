;;; typespec-ts-mode.el --- Major mode for TypeSpec (using tree-sitter) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Pradyuman Vig
;;
;; Author: Pradyuman Vig <me@pmn.co>
;; Created: 19 January 2025
;; Modified: 1 August 2026
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages tree-sitter typespec
;; URL: https://github.com/pradyuman/typespec-ts-mode
;; SPDX-License-Identifier: MIT
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides a major mode for editing TypeSpec files
;; using tree-sitter. It is compatible with the grammar at
;; https://github.com/happenslol/tree-sitter-typespec.
;;
;;; Code:

(require 'treesit)
(require 'c-ts-common)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'rx))

;;; Font lock
(defvar typespec-ts-mode--font-lock-feature-list
  '((comment definition)
    (escape keyword string type)
    (constant decorator directive namespace number)
    (bracket delimiter operator property)))

(defvar typespec-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Keep punctuation classes aligned with the grammar's highlight query:
   ;; https://github.com/happenslol/tree-sitter-typespec/blob/main/queries/highlights.scm
   :language 'typespec
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}" "<" ">" "#{" "#["] @font-lock-bracket-face)

   :language 'typespec
   :feature 'comment
   '((single_line_comment) @font-lock-comment-face
     (multi_line_comment) @font-lock-comment-face)

   :language 'typespec
   :feature 'constant
   '(["true" "false" "null"] @font-lock-constant-face
     (enum_member name: (identifier) @font-lock-constant-face))

   :language 'typespec
   :feature 'decorator
   '((decorator
      "@" @font-lock-builtin-face
      name: (identifier_or_member_expression
             [(identifier) @font-lock-builtin-face
              (member_expression
               base: (identifier) @font-lock-builtin-face
               member: (identifier) @font-lock-builtin-face)]))
     (augment_decorator_statement
      "@@" @font-lock-builtin-face
      name: (identifier_or_member_expression
             [(identifier) @font-lock-builtin-face
              (member_expression
               base: (identifier) @font-lock-builtin-face
               member: (identifier) @font-lock-builtin-face)])))

   :language 'typespec
   :feature 'definition
   '((builtin_type) @font-lock-type-face
     (alias_statement name: (identifier) @font-lock-type-face)
     (const_statement name: (identifier) @font-lock-constant-face)
     (enum_statement name: (identifier) @font-lock-type-face)
     (interface_statement name: (identifier) @font-lock-type-face)
     (model_statement name: (identifier) @font-lock-type-face)
     (operation_statement name: (identifier) @font-lock-function-name-face)
     (scalar_statement name: (identifier) @font-lock-type-face)
     (union_statement name: (identifier) @font-lock-type-face))

   :language 'typespec
   :feature 'delimiter
   '(["," ";" "." ":"] @font-lock-delimiter-face
     "?" @font-lock-misc-punctuation-face)

   :language 'typespec
   :feature 'directive
   '((directive
      "#" @font-lock-warning-face
      (identifier_or_member_expression
       [(identifier) @font-lock-warning-face
        (member_expression
         base: (identifier) @font-lock-warning-face
         member: (identifier) @font-lock-warning-face)])))

   :language 'typespec
   :feature 'escape
   '((escape_sequence) @font-lock-escape-face)

   :language 'typespec
   :feature 'keyword
   ;; https://github.com/microsoft/typespec/blob/main/packages/spec/src/spec.emu.html#L34
   '(["import" "model" "namespace" "op" "extends" "using" "interface" "union"
      "dec" "fn" "void" "never" "unknown" "alias" "enum" "scalar" "is" "const"
      "typeof" "valueof" (decorator_modifiers) (function_modifiers)]
     @font-lock-keyword-face)

   :language 'typespec
   :feature 'namespace
   '((namespace_statement
      name: (identifier_or_member_expression
             [(identifier) @font-lock-function-name-face
              (member_expression
               base: (identifier) @font-lock-function-name-face
               member: (identifier) @font-lock-function-name-face)]))
     (using_statement
      module: (identifier_or_member_expression
               [(identifier) @font-lock-function-name-face
                (member_expression
                 base: (identifier) @font-lock-function-name-face
                 member: (identifier) @font-lock-function-name-face)])))

   :language 'typespec
   :feature 'number
   '([(decimal_literal) (hex_integer_literal) (binary_integer_literal)]
     @font-lock-number-face)

   :language 'typespec
   :feature 'operator
   '(["|" "&" "=" "..."] @font-lock-operator-face)

   :language 'typespec
   :feature 'property
   '((model_property name: [(identifier) @font-lock-property-name-face])
     (union_variant name: (identifier) @font-lock-property-name-face))

   :language 'typespec
   :feature 'string
   '((quoted_string_literal) @font-lock-string-face
     (triple_quoted_string_literal) @font-lock-string-face)

   :language 'typespec
   :feature 'type
   '((template_parameter name: (identifier) @font-lock-type-face)
     (reference_expression
      ((identifier_or_member_expression
        [(identifier) @font-lock-type-face
         (member_expression
          base: (identifier) @font-lock-type-face
          member: (identifier) @font-lock-type-face)]))))))

;;; Indent
(defcustom typespec-ts-mode-indent-offset 2
  "Number of spaces for each indentation step."
  :type 'integer
  :safe 'integerp
  :group 'typespec)

(defun typespec-ts-mode--multi-line-string-indent (_node _parent bol &rest _rest)
  "Return the indent for the current multi-line string line at BOL.

This is either the current indentation or the indentation of the closing triple
quotes, whichever is greater."
  (let ((node (treesit-node-at (point))))
    (when (and node (string= (treesit-node-type node) "triple_quoted_string_fragment"))
      (let ((minimum-indentation (save-excursion
                                   (goto-char (treesit-node-end node))
                                   (current-indentation))))
        (max minimum-indentation bol)))))

(defun typespec-ts-mode--model-expression-anchor (_node parent _bol &rest _rest)
  "Return the indentation anchor for a model expression with PARENT.

For a model expression following a union variant marker, use the opening brace.
Otherwise, use the indentation of the line containing the model expression."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (let ((parent-start (point)))
      (back-to-indentation)
      (if (eq (char-after) ?|)
          parent-start
        (point)))))

(defun typespec-ts-mode--union-expression-anchor (_node parent _bol &rest _rest)
  "Return the indentation anchor for a union expression with PARENT."
  (save-excursion
    (while (and parent (string= (treesit-node-type parent) "union_expression"))
      (setq parent (treesit-node-parent parent)))
    (goto-char (treesit-node-start parent))
    (back-to-indentation)
    (point)))

(defvar typespec-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?` "\"" table)
    ;; comments like c-mode(s)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `typespec-ts-mode'.")

(defvar typespec-ts-mode--indent-rules
  '((typespec
     ((parent-is "source_file") parent 0)
     ((and (node-is "enum") (parent-is "enum_statement")) standalone-parent 0)
     ((and (node-is "}") (parent-is "model_expression"))
      typespec-ts-mode--model-expression-anchor 0)
     ((and (node-is "}") (parent-is "interface_body")) standalone-parent 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((and (parent-is "multi_line_comment") c-ts-common-looking-at-star) c-ts-common-comment-start-after-first-star -1)
     ((parent-is "single_line_comment") prev-adaptive-prefix 0)
     ((parent-is "multi_line_comment") prev-adaptive-prefix 0)
     ((parent-is "union_expression") typespec-ts-mode--union-expression-anchor typespec-ts-mode-indent-offset)
     ((parent-is "union_body") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "operation_arguments") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "model_body") parent-bol 0)
     ((parent-is "model_expression") typespec-ts-mode--model-expression-anchor typespec-ts-mode-indent-offset)
     ((parent-is "object_value") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "value_list") parent-bol 0)
     ((parent-is "triple_quoted_string_fragment") typespec-ts-mode--multi-line-string-indent 0)
     ((parent-is "interface_body") standalone-parent typespec-ts-mode-indent-offset))))

(defun typespec-ts-mode--defun-name (node)
  "Find name of NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

;;;###autoload
(define-derived-mode typespec-ts-mode prog-mode "TypeSpec"
  "Major mode for editing TypeSpec files."
  :group 'typespec
  :syntax-table typespec-ts-mode--syntax-table

  (unless (treesit-available-p)
    (error "Tree-sitter is not available"))

  (treesit-parser-create 'typespec)

  ;; Comments
  (c-ts-common-comment-setup)

  ;; Font Lock
  (setq-local treesit-font-lock-feature-list typespec-ts-mode--font-lock-feature-list
              treesit-font-lock-settings typespec-ts-mode--font-lock-settings)

  ;; Indent
  (setq-local treesit-simple-indent-rules typespec-ts-mode--indent-rules)
  (setq-local electric-indent-chars (append "{}" electric-indent-chars))

  ;; imenu
  (setq-local treesit-defun-type-regexp (regexp-opt
                                         '("alias_statement"
                                           "const_statement"
                                           "enum_statement"
                                           "interface_statement"
                                           "namespace_statement"
                                           "operation_statement"
                                           "scalar_statement"
                                           "union_statement"
                                           "model_statement")))
  (setq-local treesit-defun-name-function #'typespec-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings
              `(("Alias" "\\`alias_statement\\'")
                ("Constant" "\\`const_statement\\'")
                ("Enum" "\\`enum_statement\\'")
                ("Interface" "\\`interface_statement\\'")
                ("Model" "\\`model_statement\\'")
                ("Namespace" "\\`namespace_statement\\'")
                ("Operation" "\\`operation_statement\\'")
                ("Scalar" "\\`scalar_statement\\'")
                ("Union" "\\`union_statement\\'")))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsp\\'" . typespec-ts-mode))

(provide 'typespec-ts-mode)
;;; typespec-ts-mode.el ends here
