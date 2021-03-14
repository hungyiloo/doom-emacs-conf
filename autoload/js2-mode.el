;;; autoload/js2-mode.el -*- lexical-binding: t; -*-

(defvar my/evil-tsc-outer-inner-js-alist
  '((function_declaration . :body)
    (generator_function_declaration . :body)
    (arrow_function . :body)
    (method_definition . :body)
    (variable_declarator . :value)
    (public_field_definition . :value)
    (assignment_expression . :right)
    (call_expression . :arguments))
  "Maps a tree-sitter outer node type to its inner node name.")

(defvar my/evil-js-text-object-tsc-node-type-alist
  '((function . (function_declaration
                 generator_function_declaration
                 arrow_function
                 method_definition
                 call_expression))
    (declaration . (lexical_declaration
                    variable_declaration
                    public_field_definition
                    assignment_expression))
    (declarator . (variable_declarator
                   public_field_definition
                   assignment_expression))
    (statement . (lexical_declaration
                  variable_declaration
                  public_field_definition
                  export_statement
                  import_statement
                  debugger_statement
                  expression_statement
                  statement_block
                  if_statement
                  switch_statement
                  for_statement
                  for_in_statement
                  while_statement
                  do_statement
                  try_statement
                  with_statement
                  break_statement
                  continue_statement
                  return_statement
                  throw_statement
                  empty_statement
                  labeled_statement)))
  "Maps a text object type to its corresponding tree-sitter node types.")

;;;###autoload
(defun my/evil-tsc-text-object (type text-object visual-p inner-p)
  (save-excursion
    (when (and (doom-region-active-p)
               (not (eq (point) (mark))))
      (when (> (point) (mark))
        (exchange-point-and-mark))
      (when (null inner-p)
        (goto-char (1- (point)))))
    (condition-case nil
        (let* ((current-node (tree-sitter-node-at-point))
               (current-node-type (tsc-node-type current-node))
               (target-node-types (alist-get text-object my/evil-js-text-object-tsc-node-type-alist)))
          (while (not (memq current-node-type target-node-types))
            (setq current-node (tsc-get-parent current-node))
            (setq current-node-type (tsc-node-type current-node)))

          (let* ((node-range (if inner-p
                                 (if-let ((child-name (alist-get current-node-type my/evil-tsc-outer-inner-js-alist)))
                                     (tsc-node-position-range (tsc-get-child-by-field current-node child-name))
                                   (let* ((child-count (tsc-count-children current-node))
                                          (first-child (tsc-get-nth-child current-node (if (> child-count 2) 1 0)))
                                          (last-child (tsc-get-nth-child current-node (- child-count (if (>= child-count 2) 2 1)))))
                                     `(,(car (tsc-node-position-range first-child)) . ,(cdr (tsc-node-position-range last-child)))))
                               (tsc-node-position-range current-node)))
                 (beg (car node-range))
                 (end (cdr node-range)))
            (evil-range
             beg
             (if visual-p (1- end) end)
             type)))
      (error (error "No suitable node found")))))

;;;###autoload (autoload 'my/evil-visual-inner-js-function "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-inner-js-function (count &optional beg end type)
  "Visual inner text object for all Javascript functions."
  (my/evil-tsc-text-object type 'function t t))
;;;###autoload (autoload 'my/evil-inner-js-function "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-inner-js-function (count &optional beg end type)
  "Inner text object for all Javascript functions."
  (my/evil-tsc-text-object type 'function nil t))
;;;###autoload (autoload 'my/evil-visual-outer-js-function "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-outer-js-function (count &optional beg end type)
  "Visual outer text object for all Javascript functions."
  (my/evil-tsc-text-object type 'function t nil))
;;;###autoload (autoload 'my/evil-outer-js-function "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-outer-js-function (count &optional beg end type)
  "Outer text object for all Javascript functions."
  (my/evil-tsc-text-object type 'function nil nil))

;;;###autoload (autoload 'my/evil-visual-inner-js-declaration "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-inner-js-declaration (count &optional beg end type)
  "Visual inner text object for all Javascript declarations."
  (my/evil-tsc-text-object type 'declarator t t))
;;;###autoload (autoload 'my/evil-inner-js-declaration "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-inner-js-declaration (count &optional beg end type)
  "Inner text object for all Javascript declarations."
  (my/evil-tsc-text-object type 'declarator nil t))
;;;###autoload (autoload 'my/evil-visual-outer-js-declaration "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-outer-js-declaration (count &optional beg end type)
  "Visual outer text object for all Javascript declarations."
  (my/evil-tsc-text-object type 'declaration t nil))
;;;###autoload (autoload 'my/evil-outer-js-declaration "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-outer-js-declaration (count &optional beg end type)
  "Outer text object for all Javascript declarations."
  (my/evil-tsc-text-object type 'declaration nil nil))

;;;###autoload (autoload 'my/evil-visual-inner-js-statement "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-inner-js-statement (count &optional beg end type)
  "Visual inner text object for all Javascript statements."
  (my/evil-tsc-text-object type 'statement t t))
;;;###autoload (autoload 'my/evil-inner-js-statement "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-inner-js-statement (count &optional beg end type)
  "Inner text object for all Javascript statements."
  (my/evil-tsc-text-object type 'statement nil t))
;;;###autoload (autoload 'my/evil-visual-outer-js-statement "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-visual-outer-js-statement (count &optional beg end type)
  "Visual outer text object for all Javascript statements."
  (my/evil-tsc-text-object type 'statement t nil))
;;;###autoload (autoload 'my/evil-outer-js-statement "autoload/js2-mode" nil t)
(evil-define-text-object
  my/evil-outer-js-statement (count &optional beg end type)
  "Outer text object for all Javascript statements."
  (my/evil-tsc-text-object type 'statement nil nil))
