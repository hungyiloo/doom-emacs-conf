;;; config/typescript-mode.el -*- lexical-binding: t; -*-

(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq tide-native-json-parsing t)
  (setq tide-completion-ignore-case t))

(add-hook! 'after-init-hook
           ;; This derived mode won't work well until tree-sitter-indent
           ;; supports typescript/tsx.
           ;;
           ;; For now it provides a faster alternative when the web-mode derived
           ;; mode from doom is too slow
           (define-derived-mode tsx-mode typescript-mode "tsx")
           (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))

(add-hook! 'tsx-mode-hook
  (setq tsx-indent-offset 2)
  ;; (setq tree-sitter-indent-tsx-scopes
  ;;       '((indent-all . ;; these nodes are always indented
  ;;                     ())
  ;;         (indent-rest . ;; if parent node is one of this and node is not first → indent
  ;;                      ;; (function array for_statement if_statement while_statement arrow_function call_expression variable_declarator)
  ;;                      (jsx_opening_element
  ;;                       jsx_self_closing_element
  ;;                       arguments
  ;;                       formal_parameters
  ;;                       parenthesized_expression
  ;;                       object
  ;;                       object_type
  ;;                       array
  ;;                       array_type
  ;;                       statement_block
  ;;                       jsx_element
  ;;                       member_expression
  ;;                       ternary_expression))
  ;;         (indent-body . ;; if parent node is one of this and current node is in middle → indent
  ;;                      (jsx_fragment))

  ;;         (paren-indent . ;; if parent node is one of these → indent to paren opener
  ;;                       ())
  ;;         ;; (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
  ;;         ;;                ((?. . (call_expression field_expression))))
  ;;         (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
  ;;                           (jsx_attribute
  ;;                            lexical_declaration
  ;;                            variable_declarator))

  ;;         (multi-line-text . ;; if node is one of this, then don't modify the indent
  ;;                          ;; this is basically a peaceful way out by saying "this looks like something
  ;;                          ;; that cannot be indented using AST, so best I leave it as-is"
  ;;                          ())
  ;;         (outdent . ;; these nodes always outdent (1 shift in opposite direction)
  ;;                  ("{" "[" "(" ")" "]" "}" jsx_closing_element)
  ;;                  ;; ()
  ;;                  )))
  (require 'tree-sitter-indent)
  ;; (tree-sitter-indent-mode 1)
  (setq electric-indent-chars '(123 125 40 41 58 59 44 10 127 61 34 39 96 91 93))
  (lsp)
  (defun my/tsx-indent-line-function ()
    (let* ((curr-point (save-excursion (beginning-of-line-text) (point)))
           (curr-column (save-excursion (beginning-of-line-text) (current-column)))
           (node (tree-sitter-indent--highest-node-at-position curr-point))
           (node-line (car (tsc-node-start-point node)))
           (node-type (tsc-node-type node))
           (curr-line (line-number-at-pos curr-point))
           (target-column (or (when-let* ((parent (if (and (memq node-type '(jsx_text))
                                                           (= curr-line node-line))
                                                      (tsc-get-parent node)
                                                    node))
                                          (parent-line (car (tsc-node-start-point parent)))
                                          (parent-out-of-line (not (= curr-line parent-line)))
                                          (parent-column (save-excursion
                                                           (goto-char (tsc-node-start-byte parent))
                                                           (beginning-of-line-text)
                                                           (current-column))))
                                (if parent-out-of-line
                                    (if (member node-type '("[" "(" "{" "}" ")" "]"
                                                            jsx_closing_element
                                                            statement_block
                                                            else_clause))
                                        (if (> curr-point (tsc-node-start-byte node))
                                            (+ parent-column tsx-indent-offset)
                                          parent-column)
                                      (+ parent-column tsx-indent-offset))
                                  curr-column))
                              0))
           (delta-column (- target-column curr-column)))

      (message "%s %s parent:%s node:%s"
               target-column
               delta-column
               (tsc-node-type (tsc-get-parent node))
               (tsc-node-type node))

      (if (> delta-column 0)
          (progn
            (save-excursion
              (beginning-of-line)
              (indent-to target-column))
            (if (search-forward-regexp "[^ \t]" (line-end-position) t)
                (backward-char)
              (end-of-line)))
        (delete-region (line-beginning-position) (+ (line-beginning-position) (abs delta-column))))))
  (cond
   (tsx-mode (setq-local indent-line-function #'my/tsx-indent-line-function))
   (t (setq-local indent-line-function (default-value 'indent-line-function)))))
