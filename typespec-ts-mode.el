;;; typespec-ts-mode.el --- Major mode for TypeSpec (using tree-sitter) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Pradyuman Vig
;;
;; Author: Pradyuman Vig <me@pmn.co>
;; Created: 19 January 2025
;; Modified: 27 January 2025
;; Version: 0.1
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
    (keyword string type)
    (constant decorator directive namespace)
    (bracket delimiter property)))

(defvar typespec-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'typespec
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'typespec
   :feature 'comment
   '((single_line_comment) @font-lock-comment-face
     (multi_line_comment) @font-lock-comment-face)

   :language 'typespec
   :feature 'constant
   '([(decimal_literal) (hex_integer_literal) (binary_integer_literal)
      "true" "false" "null"] @font-lock-constant-face)

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
     (enum_statement name: (identifier) @font-lock-type-face)
     (interface_statement name: (identifier) @font-lock-type-face)
     (model_statement name: (identifier) @font-lock-type-face)
     (operation_statement name: (identifier) @font-lock-function-name-face)
     (scalar_statement name: (identifier) @font-lock-type-face)
     (union_statement name: (identifier) @font-lock-type-face))

   :language 'typespec
   :feature 'delimiter
   '(["<" ">"] @font-lock-delimiter-face)

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
      (decorator_modifiers) (function_modifiers)] @font-lock-keyword-face)

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
   :feature 'property
   '((enum_member name: [(identifier) @font-lock-property-name-face])
     (model_property name: [(identifier) @font-lock-property-name-face])
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

(defun typespec-ts-mode--multi-line-string-indent (n parent bol &rest rest)
  "Return the indent for the current multi-line string line.

This is either the current indentation or the indentation of the closing triple
quotes, whichever is greater.
"
  (let ((node (treesit-node-at (point))))
    (when (and node (string= (treesit-node-type node) "triple_quoted_string_fragment"))
      (let ((minimum-identation (save-excursion
                                  (goto-char (treesit-node-end node))
                                  (current-indentation))))

        (max minimum-identation bol)))))

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
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((and (parent-is "multi_line_comment") c-ts-common-looking-at-star) c-ts-common-comment-start-after-first-star -1)
     ((parent-is "single_line_comment") prev-adaptive-prefix 0)
     ((parent-is "multi_line_comment") prev-adaptive-prefix 0)
     ((parent-is "union_body") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "operation_arguments") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "model_body") parent-bol 0)
     ((parent-is "model_expression") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "object_value") parent-bol typespec-ts-mode-indent-offset)
     ((parent-is "value_list") parent-bol 0)
     ((parent-is "string_fragment") nix-ts-indent-multiline-string 0)
     ((parent-is "triple_quoted_string_fragment") typespec-ts-mode--multi-line-string-indent 0)
     ((parent-is "interface_body") parent-bol typespec-ts-mode-indent-offset))))

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
  (setq-local treesit-defun-name-function #'typespec-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings
              `(("Alias" "\\`alias_statement\\'")
                ("Enum" "\\`enum_statement\\'")
                ("Interface" "\\`interface_statement\\'")
                ("Model" "\\`model_statement\\'")
                ("Operation" "\\`operation_statement\\'")
                ("Scalar" "\\`scalar_statement\\'")
                ("Union" "\\`union_statement\\'")))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsp\\'" . typespec-ts-mode))

(provide 'typespec-ts-mode)
;;; typespec-ts-mode.el ends here
