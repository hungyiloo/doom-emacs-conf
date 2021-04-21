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

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))

(defun my/tsc-highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\)."
  (save-excursion
    (goto-char position)
    (let* ((current-node (tree-sitter-node-at-point))
           (parent-node (tsc-get-parent current-node)))
      (while (and parent-node
                  (eq (tsc-node-start-byte parent-node)
                      (tsc-node-start-byte current-node)))
        (setq current-node parent-node)
        (setq parent-node (tsc-get-parent current-node)))
      current-node)))

(defun my/tsx-indent-line-function ()
  (let* ((curr-point (save-excursion (back-to-indentation) (point)))
         (curr-column (current-indentation))
         (node (my/tsc-highest-node-at-position curr-point))
         (node-line (car (tsc-node-start-point node)))
         (node-type (tsc-node-type node))
         (curr-line (line-number-at-pos curr-point))
         (container (if (or (memq node-type '(jsx_text))
                            (= curr-line node-line))
                        (or (tsc-get-parent node) node)
                      node))
         (container-column (save-excursion
                             (goto-char (car (tsc-node-position-range container)))
                             (current-indentation)))
         (target-column (cond
                         ((eq 'program (tsc-node-type container)) 0)
                         ((member node-type '("[" "(" "{" "}" ")" "]" "<" "/"
                                              jsx_closing_element
                                              statement_block
                                              else_clause))
                          (if (and (> curr-line node-line)
                                   (< curr-line (car (tsc-node-end-point node))))
                              (+ container-column js-indent-level)
                            container-column))
                         ((> curr-point (car (tsc-node-position-range container)))
                          (+ container-column js-indent-level))
                         (t curr-column))))

    ;; (message "%s container:%s%s node:%s%s"
    ;;          target-column
    ;;          (tsc-node-type container)
    ;;          (tsc-node-start-point container)
    ;;          (tsc-node-type node)
    ;;          (tsc-node-start-point node))

    (save-excursion (indent-line-to target-column))
    (skip-chars-forward " \t\n" (line-end-position))))

(add-hook! 'tsx-mode-hook
  (tree-sitter-require 'tsx)
  (tree-sitter-hl-add-patterns nil "[\"/\" \"*\"] @operator")
  (emmet-mode 1)
  (lsp)
  (cond
   ((eq major-mode 'tsx-mode) (setq-local indent-line-function #'my/tsx-indent-line-function))
   (t (setq-local indent-line-function (default-value 'indent-line-function)))))
