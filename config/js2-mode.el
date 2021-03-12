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
  (defun my/evil-select-js-function (type visual-p inner-p)
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
          (let ((current-node (tree-sitter-node-at-point)))
            (while (or (null current-node)
                       (not (memq
                             (tsc-node-type current-node)
                             '(function_declaration
                               generator_function_declaration
                               arrow_function
                               method_definition))))
              (setq current-node (tsc-get-parent current-node)))
            (when inner-p
              (setq current-node (tsc-get-child-by-field current-node :body)))
            (let* ((node-range (tsc-node-position-range current-node))
                   (beg (car node-range))
                   (end (cdr node-range)))
              (evil-range
               beg
               (if visual-p (1- end) end)
               type)))
        (error (error "No surrounding function found")))))

  (evil-define-text-object
    evil-visual-inner-js-function (count &optional beg end type)
    "Visual inner text object for all Javascript functions."
    (my/evil-select-js-function type t t))
  (evil-define-text-object
    evil-inner-js-function (count &optional beg end type)
    "Inner text object for all Javascript functions."
    (my/evil-select-js-function type nil t))
  (evil-define-text-object
    evil-visual-outer-js-function (count &optional beg end type)
    "Visual outer text object for all Javascript functions."
    (my/evil-select-js-function type t nil))
  (evil-define-text-object
    evil-outer-js-function (count &optional beg end type)
    "Outer text object for all Javascript functions."
    (my/evil-select-js-function type nil nil))

  ;; Hook to install the above functions
  (add-hook! (javascript-mode js-mode js2-mode typescript-mode)
    (map! :map evil-operator-state-local-map
          "af" #'evil-outer-js-function
          "if" #'evil-inner-js-function
          :map evil-visual-state-local-map
          "af" #'evil-visual-outer-js-function
          "if" #'evil-visual-inner-js-function)))
