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
  (let* ((curr-point (save-excursion (beginning-of-line-text) (point)))
         (curr-column (save-excursion (beginning-of-line-text) (current-column)))
         (node (tree-sitter-indent--highest-node-at-position curr-point))
         (node-line (car (tsc-node-start-point node)))
         (node-type (tsc-node-type node))
         (curr-line (line-number-at-pos curr-point))
         (target-column (or (when-let* ((parent (if (or (memq node-type '(jsx_text))
                                                        (= curr-line node-line))
                                                    (tsc-get-parent node)
                                                  node))
                                        (parent (unless (eq 'program (tsc-node-type parent)) parent))
                                        (parent-line (car (tsc-node-start-point parent)))
                                        (parent-out-of-line (not (= curr-line parent-line)))
                                        (parent-column (save-excursion
                                                         (goto-char (tsc-node-start-byte parent))
                                                         (beginning-of-line-text)
                                                         (current-column))))
                              (if parent-out-of-line
                                  (if (member node-type '("[" "(" "{" "}" ")" "]" "<"
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

    ;; (message "%s %s parent:%s node:%s"
    ;;          target-column
    ;;          delta-column
    ;;          (tsc-node-type (tsc-get-parent node))
    ;;          (tsc-node-type node))

    (if (> delta-column 0)
        (progn
          (save-excursion
            (beginning-of-line)
            (indent-to target-column))
          (if (search-forward-regexp "[^ \t]" (line-end-position) t)
              (backward-char)
            (end-of-line)))
      (delete-region (line-beginning-position) (+ (line-beginning-position) (abs delta-column))))))

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
