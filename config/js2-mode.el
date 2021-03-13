;;; config/js2-mode.el -*- lexical-binding: t; -*-

(after! js2-mode
  ;; Fix some edge case javascript indenting
  (setq js-indent-level 2)

  ;; Fix some farty prettify-symbols-mode quirks in JavaScript.
  ;; Ligature fonts already handle =>, <= and >=
  ;; so I don't need emacs's prettification for them.
  (after! js
    (setq js--prettify-symbols-alist nil)))

(after! (:and evil (:or typescript-mode js2-mode))
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

  (defun my/evil-tsc-text-object (type text-object visual-p inner-p)
    (save-excursion
      (when (doom-region-active-p)
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

  (evil-define-text-object
    my/evil-visual-inner-js-function (count &optional beg end type)
    "Visual inner text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function t t))
  (evil-define-text-object
    my/evil-inner-js-function (count &optional beg end type)
    "Inner text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function nil t))
  (evil-define-text-object
    my/evil-visual-outer-js-function (count &optional beg end type)
    "Visual outer text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function t nil))
  (evil-define-text-object
    my/evil-outer-js-function (count &optional beg end type)
    "Outer text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function nil nil))

  (evil-define-text-object
    my/evil-visual-inner-js-declaration (count &optional beg end type)
    "Visual inner text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declarator t t))
  (evil-define-text-object
    my/evil-inner-js-declaration (count &optional beg end type)
    "Inner text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declarator nil t))
  (evil-define-text-object
    my/evil-visual-outer-js-declaration (count &optional beg end type)
    "Visual outer text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declaration t nil))
  (evil-define-text-object
    my/evil-outer-js-declaration (count &optional beg end type)
    "Outer text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declaration nil nil))

  (evil-define-text-object
    my/evil-visual-inner-js-statement (count &optional beg end type)
    "Visual inner text object for all Javascript statements."
    (my/evil-tsc-text-object type 'statement t t))
  (evil-define-text-object
    my/evil-inner-js-statement (count &optional beg end type)
    "Inner text object for all Javascript statements."
    (my/evil-tsc-text-object type 'statement nil t))
  (evil-define-text-object
    my/evil-visual-outer-js-statement (count &optional beg end type)
    "Visual outer text object for all Javascript statements."
    (my/evil-tsc-text-object type 'statement t nil))
  (evil-define-text-object
    my/evil-outer-js-statement (count &optional beg end type)
    "Outer text object for all Javascript statements."
    (my/evil-tsc-text-object type 'statement nil nil))

  ;; Hook to install the above functions
  (add-hook! (javascript-mode js-mode js2-mode typescript-mode)
    (map! :map evil-operator-state-local-map
          "af" #'my/evil-outer-js-function
          "if" #'my/evil-inner-js-function
          "ad" #'my/evil-outer-js-declaration
          "id" #'my/evil-inner-js-declaration
          "as" #'my/evil-outer-js-statement
          "is" #'my/evil-inner-js-statement
          :map evil-visual-state-local-map
          "af" #'my/evil-visual-outer-js-function
          "if" #'my/evil-visual-inner-js-function
          "ad" #'my/evil-visual-outer-js-declaration
          "id" #'my/evil-visual-inner-js-declaration
          "as" #'my/evil-visual-outer-js-statement
          "is" #'my/evil-visual-inner-js-statement)))
