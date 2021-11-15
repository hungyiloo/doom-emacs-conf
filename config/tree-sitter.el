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

  (after! tree-sitter-hl
    (custom-set-faces!
      ;; Fix blotches of wrong background color in org src blocks
      `(tree-sitter-hl-face:punctuation.bracket :inherit nil)
      `(tree-sitter-hl-face:punctuation.delimiter :inherit nil)
      `(tree-sitter-hl-face:punctuation :inherit nil)
      ;; Fix embedded code in strings to be distinguishable
      `(tree-sitter-hl-face:embedded :weight bold)))

  (defun my/tree-sitter-point-in-comment-p (&optional pos)
    "Use tree-sitter to determine if point is inside comment"
    (let ((pos (or pos (point))))
      (and (not (= (point-min) pos))
           (save-excursion
             (goto-char (1- pos))
             (tree-sitter-node-at-point 'comment nil t)))))

  ;; Don't really need to add this hook if we're already overriding `doom-point-in-comment-p'
  ;; (add-hook! 'doom-point-in-comment-functions #'my/tree-sitter-point-in-comment-p)
  (advice-add #'doom-point-in-comment-p
              :around
              ;; Use tree-sitter instead of smartparens to detect if we're in a comment
              ;; Hooray for tree-sitter!
              (defun my/doom-point-in-comment-p-override (orig-fun &rest args)
                (apply (if tree-sitter-mode
                             #'my/tree-sitter-point-in-comment-p
                           orig-fun)
                       args))))
