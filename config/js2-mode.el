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
      (public_field_definition . :value))
    "Maps a tree-sitter outer node type to its inner node name.")

  (defvar my/evil-js-text-object-tsc-node-type-alist
    '((function . (function_declaration
                   generator_function_declaration
                   arrow_function
                   method_definition))
      (declaration . (lexical_declaration
                      variable_declaration
                      public_field_definition))
      (declarator . (variable_declarator
                     public_field_definition)))
    "Maps a text object type to its corresponding tree-sitter node types.")

  (defun my/evil-tsc-text-object (type text-object visual-p inner-p)
    (save-excursion
      (when (and (doom-region-active-p)
                 (> (point) (mark)))
        (let ((prev-point (point))
              (prev-mark (mark)))
          (set-mark prev-point)
          (goto-char prev-mark)))
      (when (and (doom-region-active-p)
                 (null inner-p))
        (goto-char (1- (point))))
      (condition-case nil
          (let* ((current-node (tree-sitter-node-at-point))
                 (current-node-type (tsc-node-type current-node))
                 (target-node-types (alist-get text-object my/evil-js-text-object-tsc-node-type-alist)))
            (while (not (memq current-node-type target-node-types))
              (setq current-node (tsc-get-parent current-node))
              (setq current-node-type (tsc-node-type current-node)))
            (when inner-p
              (setq current-node
                    (tsc-get-child-by-field
                     current-node
                     (alist-get current-node-type my/evil-tsc-outer-inner-js-alist))))
            (let* ((node-range (tsc-node-position-range current-node))
                   (beg (car node-range))
                   (end (cdr node-range)))
              (evil-range
               beg
               (if visual-p (1- end) end)
               type)))
        (error (error "No suitable node found")))))

  (evil-define-text-object
    evil-visual-inner-js-function (count &optional beg end type)
    "Visual inner text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function t t))
  (evil-define-text-object
    evil-inner-js-function (count &optional beg end type)
    "Inner text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function nil t))
  (evil-define-text-object
    evil-visual-outer-js-function (count &optional beg end type)
    "Visual outer text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function t nil))
  (evil-define-text-object
    evil-outer-js-function (count &optional beg end type)
    "Outer text object for all Javascript functions."
    (my/evil-tsc-text-object type 'function nil nil))

  (evil-define-text-object
    evil-visual-inner-js-declaration (count &optional beg end type)
    "Visual inner text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declarator t t))
  (evil-define-text-object
    evil-inner-js-declaration (count &optional beg end type)
    "Inner text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declarator nil t))
  (evil-define-text-object
    evil-visual-outer-js-declaration (count &optional beg end type)
    "Visual outer text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declaration t nil))
  (evil-define-text-object
    evil-outer-js-declaration (count &optional beg end type)
    "Outer text object for all Javascript declarations."
    (my/evil-tsc-text-object type 'declaration nil nil))

  ;; Hook to install the above functions
  (add-hook! (javascript-mode js-mode js2-mode typescript-mode)
    (map! :map evil-operator-state-local-map
          "af" #'evil-outer-js-function
          "if" #'evil-inner-js-function
          "ad" #'evil-outer-js-declaration
          "id" #'evil-inner-js-declaration
          :map evil-visual-state-local-map
          "af" #'evil-visual-outer-js-function
          "if" #'evil-visual-inner-js-function
          "ad" #'evil-visual-outer-js-declaration
          "id" #'evil-visual-inner-js-declaration)))
