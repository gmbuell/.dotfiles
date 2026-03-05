;;; pkl-ts-mode.el --- tree-sitter support for PKL  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The Pkl Authors

;; Author     : Garret
;; Keywords   : pkl languages tree-sitter

;;; Commentary:

;; Tree-sitter-based major mode for editing PKL (Apple's configuration
;; language).  Requires Emacs 29+ with tree-sitter support and the
;; tree-sitter-pkl grammar installed.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defgroup pkl nil
  "Support for PKL."
  :group 'languages)

(defcustom pkl-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `pkl-ts-mode'."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'pkl)

(defvar pkl-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_   "_"       table)
    (modify-syntax-entry ?\\  "\\"      table)
    (modify-syntax-entry ?+   "."       table)
    (modify-syntax-entry ?-   "."       table)
    (modify-syntax-entry ?=   "."       table)
    (modify-syntax-entry ?%   "."       table)
    (modify-syntax-entry ?&   "."       table)
    (modify-syntax-entry ?|   "."       table)
    (modify-syntax-entry ?!   "."       table)
    (modify-syntax-entry ?<   "."       table)
    (modify-syntax-entry ?>   "."       table)
    (modify-syntax-entry ?~   "."       table)
    (modify-syntax-entry ?@   "."       table)
    (modify-syntax-entry ?/   ". 124b"  table)
    (modify-syntax-entry ?*   ". 23"    table)
    (modify-syntax-entry ?\n  "> b"     table)
    (modify-syntax-entry ?\^m "> b"     table)
    table)
  "Syntax table for `pkl-ts-mode'.")

;;; Keywords and operators

(defvar pkl-ts-mode--keywords
  '("abstract" "amends" "as" "class" "else" "extends" "external"
    "for" "function" "hidden" "if" "import" "import*" "in" "is"
    "let" "local" "module" "new" "open" "out" "typealias" "when")
  "PKL keywords for tree-sitter font-locking.")

(defvar pkl-ts-mode--operators
  '("??" "=" "<" ">" "!" "==" "!=" "<=" ">=" "&&" "||"
    "+" "-" "**" "*" "/" "~/" "%" "|>")
  "PKL operators for tree-sitter font-locking.")

(defvar pkl-ts-mode--builtin-functions
  '("read" "read?" "read*" "throw" "trace")
  "PKL built-in functions for tree-sitter font-locking.")

;;; Indentation

(defvar pkl-ts-mode--indent-rules
  `((pkl
     ((parent-is "module") column-0 0)

     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "else") parent-bol 0)

     ((parent-is "mlStringLiteralExpr") no-indent 0)

     ((parent-is "objectBody") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "classBody") parent-bol pkl-ts-mode-indent-offset)

     ((parent-is "ifExpr") parent-bol pkl-ts-mode-indent-offset)

     ((parent-is "importClause") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "importGlobClause") parent-bol pkl-ts-mode-indent-offset)

     ((parent-is "argumentList") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "parameterList") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "typeArgumentList") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "typeParameterList") parent-bol pkl-ts-mode-indent-offset)

     ((parent-is "moduleHeader") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "moduleClause") parent-bol pkl-ts-mode-indent-offset)
     ((parent-is "extendsOrAmendsClause") parent-bol pkl-ts-mode-indent-offset)

     ((parent-is "annotation") parent-bol pkl-ts-mode-indent-offset)

     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `pkl-ts-mode'.")

;;; Font-lock

(defvar pkl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Comments
   :language 'pkl
   :feature 'comment
   '((lineComment) @font-lock-comment-face
     (blockComment) @font-lock-comment-face
     (docComment) @font-lock-doc-face)

   ;; Strings
   :language 'pkl
   :feature 'string
   '((slStringLiteralExpr) @font-lock-string-face
     (mlStringLiteralExpr) @font-lock-string-face
     (stringConstant) @font-lock-string-face)

   ;; Escape sequences and string interpolation delimiters
   :language 'pkl
   :feature 'escape-sequence
   :override t
   '((escapeSequence) @font-lock-escape-face
     (stringInterpolation "\\(" @font-lock-escape-face)
     (stringInterpolation "\\#(" @font-lock-escape-face)
     (stringInterpolation "\\##(" @font-lock-escape-face)
     (stringInterpolation ")" @font-lock-escape-face))

   ;; Keywords
   :language 'pkl
   :feature 'keyword
   `([,@pkl-ts-mode--keywords] @font-lock-keyword-face)

   ;; Constants
   :language 'pkl
   :feature 'constant
   '((trueLiteralExpr) @font-lock-constant-face
     (falseLiteralExpr) @font-lock-constant-face
     (nullLiteralExpr) @font-lock-constant-face)

   ;; Numbers
   :language 'pkl
   :feature 'number
   '((intLiteralExpr) @font-lock-number-face
     (floatLiteralExpr) @font-lock-number-face)

   ;; Types
   :language 'pkl
   :feature 'type
   '((clazz (identifier) @font-lock-type-face)
     (typeAlias (identifier) @font-lock-type-face)
     (declaredType (qualifiedIdentifier (identifier) @font-lock-type-face))
     (nothingType) @font-lock-type-face
     (unknownType) @font-lock-type-face
     (moduleType) @font-lock-type-face)

   ;; Definitions (methods, parameters, import aliases)
   :language 'pkl
   :feature 'definition
   '((classMethod (methodHeader (identifier)) @font-lock-function-name-face)
     (objectMethod (methodHeader (identifier)) @font-lock-function-name-face)
     (classProperty (identifier) @font-lock-property-name-face)
     (objectProperty (identifier) @font-lock-property-name-face)
     (typedIdentifier (identifier) @font-lock-variable-name-face)
     (blankIdentifier) @font-lock-variable-name-face
     (importClause (identifier) @font-lock-variable-name-face))

   ;; Function calls
   :language 'pkl
   :feature 'function
   :override t
   '((qualifiedAccessExpr
      (identifier) @font-lock-function-call-face (argumentList))
     (unqualifiedAccessExpr
      (identifier) @font-lock-function-call-face (argumentList)))

   ;; Built-in functions and variables
   :language 'pkl
   :feature 'builtin
   `([,@pkl-ts-mode--builtin-functions] @font-lock-builtin-face
     (thisExpr) @font-lock-builtin-face
     (outerExpr) @font-lock-builtin-face
     "super" @font-lock-builtin-face)

   ;; Annotations
   :language 'pkl
   :feature 'attribute
   '((annotation
      "@" @font-lock-preprocessor-face
      (qualifiedIdentifier (identifier) @font-lock-preprocessor-face)))

   ;; Property access
   :language 'pkl
   :feature 'property
   '((unqualifiedAccessExpr (identifier) @font-lock-property-use-face)
     (qualifiedAccessExpr (identifier) @font-lock-property-use-face))

   ;; Operators
   :language 'pkl
   :feature 'operator
   `([,@pkl-ts-mode--operators] @font-lock-operator-face
     ["?" "|" "->"] @font-lock-operator-face)

   ;; Brackets
   :language 'pkl
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face
     (typeArgumentList "<" @font-lock-bracket-face
                       ">" @font-lock-bracket-face))

   ;; Delimiters
   :language 'pkl
   :feature 'delimiter
   '(["," ":" "." "?." "..." "...?"] @font-lock-delimiter-face)

   ;; Errors
   :language 'pkl
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Font-lock settings for PKL.")

;;; Navigation

(defun pkl-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when-let* ((id (treesit-search-subtree node "identifier" nil nil 2)))
    (treesit-node-text id t)))

;;; Major mode

;;;###autoload
(define-derived-mode pkl-ts-mode prog-mode "PKL"
  "Major mode for editing PKL, powered by tree-sitter."
  :group 'pkl
  :syntax-table pkl-ts-mode--syntax-table

  (unless (treesit-ready-p 'pkl)
    (error "Tree-sitter for PKL isn't available"))

  (treesit-parser-create 'pkl)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Electric.
  (setq-local electric-indent-chars
              (append "{}()[]" electric-indent-chars))

  ;; Indent.
  (setq-local treesit-simple-indent-rules pkl-ts-mode--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("clazz" "typeAlias" "classMethod" "objectMethod"
                            "classProperty" "objectProperty")))
  (setq-local treesit-defun-name-function #'pkl-ts-mode--defun-name)
  (setq-local treesit-thing-settings
              `((pkl
                 (sentence ,(regexp-opt '("classProperty" "objectProperty"
                                          "classMethod" "objectMethod"))))))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings pkl-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute builtin constant escape-sequence function number)
                (bracket delimiter error operator property)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`clazz\\'" nil nil)
                ("Type Alias" "\\`typeAlias\\'" nil nil)
                ("Method" "\\`\\(?:class\\|object\\)Method\\'" nil nil)
                ("Property" "\\`\\(?:class\\|object\\)Property\\'" nil nil)))

  (treesit-major-mode-setup))

(if (treesit-ready-p 'pkl t)
    (progn
      (add-to-list 'auto-mode-alist '("\\.pkl\\'" . pkl-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.pcf\\'" . pkl-ts-mode))
      (add-to-list 'auto-mode-alist '("/PklProject\\'" . pkl-ts-mode))))

(provide 'pkl-ts-mode)

;;; pkl-ts-mode.el ends here
