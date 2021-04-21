;;; config/tsx-mode.el -*- lexical-binding: t; -*-

(add-hook! 'after-init-hook
           ;; This derived mode won't work well until tree-sitter-indent
           ;; supports typescript/tsx.
           ;;
           ;; For now it provides a faster alternative when the web-mode derived
           ;; mode from doom is too slow
           (define-derived-mode tsx-mode typescript-mode "tsx")
           (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

           (defun my/tsx-mode-setup ()
             (tree-sitter-require 'tsx)
             (tree-sitter-hl-add-patterns nil "[\"/\" \"*\"] @operator")
             (emmet-mode 1)
             (lsp)
             (map! :map 'tsx-mode-map
                   :i "/" #'tsx-mode-auto-close-element-maybe-h)
             (cond
              ((eq major-mode 'tsx-mode) (setq-local indent-line-function #'tsx-indent-line-function))
              (t (setq-local indent-line-function (default-value 'indent-line-function)))))

           (add-hook! 'tsx-mode-hook
             (my/tsx-mode-setup)))

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))
