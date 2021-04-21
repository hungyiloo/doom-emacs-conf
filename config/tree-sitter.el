;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :hook (after-init . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;; This enables tsx tree-sitter features but highlighting doesn't work.
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-hook! 'typescript-tsx-mode-hook (tree-sitter-require 'tsx))
  ;; Disable tree-sitter-hl-mode on typescript-tsx-mode, because of the above
  ;; (advice-remove #'tree-sitter-hl-mode #'my/tree-sitter-hl-disable-tsx)
  (advice-add #'tree-sitter-hl-mode
              :around
              (defun my/tree-sitter-hl-disable-tsx (orig-fun &rest args)
                (unless (eq major-mode 'typescript-tsx-mode)
                  (apply orig-fun args))))

  (custom-set-faces!
    ;; Fix blotches of wrong background color in org src blocks
    `(tree-sitter-hl-face:punctuation :inherit nil)))
