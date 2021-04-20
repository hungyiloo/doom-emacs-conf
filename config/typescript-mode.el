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

(defun my/tsx-indent-line-function ()
  (let* ((curr-point (save-excursion (back-to-indentation) (point)))
         (curr-column (current-indentation))
         (node (tree-sitter-indent--highest-node-at-position curr-point))
         (node-line (car (tsc-node-start-point node)))
         (node-type (tsc-node-type node))
         (curr-line (line-number-at-pos curr-point))
         (container (if (or (memq node-type '(jsx_text))
                            (= curr-line node-line))
                        (or (tsc-get-parent node) node)
                      node))
         (container-line (car (tsc-node-start-point container)))
         (container-out-of-line (not (= curr-line container-line)))
         (container-column (save-excursion
                             (goto-char (tsc-node-start-byte container))
                             (current-indentation)))
         (target-column (cond
                         ((eq 'program (tsc-node-type container)) 0)
                         ((member node-type '("[" "(" "{" "}" ")" "]" "<"
                                              jsx_closing_element
                                              statement_block
                                              else_clause))
                          (if (and (> curr-line node-line)
                                   (< curr-line (car (tsc-node-end-point node))))
                              (+ container-column tsx-indent-offset)
                            container-column))
                         ((> curr-point (tsc-node-start-byte container))
                          (+ container-column tsx-indent-offset))
                         (t curr-column))))

    ;; (message "%s container:%s node:%s"
    ;;          target-column
    ;;          (tsc-node-type container)
    ;;          (tsc-node-type node))

    (save-excursion (indent-line-to target-column))
    (skip-chars-forward " \t\n" (line-end-position))))

(add-hook! 'tsx-mode-hook
  (setq tsx-indent-offset 2)
  (require 'tree-sitter-indent)
  (setq electric-indent-chars '(123 125 40 41 58 59 44 10 127 61 34 39 96 91 93))
  (require 'emmet-mode)
  (emmet-mode 1)
  (lsp)
  (cond
   (tsx-mode (setq-local indent-line-function #'my/tsx-indent-line-function))
   (t (setq-local indent-line-function (default-value 'indent-line-function)))))
