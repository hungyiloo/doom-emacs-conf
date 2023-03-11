;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(after! tree-sitter
  (defadvice! doom-tree-sitter-point-in-comment-p-override (orig-fun &rest args)
    :around #'doom-point-in-comment-p
    ;; Use tree-sitter instead of smartparens to detect if we're in a comment
    (apply (if tree-sitter-mode
               #'doom-tree-sitter-point-in-comment-p
             orig-fun)
           args))


  ;; Don't disable tree-sitter-hl-mode anywhere
  (setq +tree-sitter-hl-enabled-modes t))

(after! tree-sitter-hl
    (custom-set-faces!
      ;; Fix blotches of wrong background color in org src blocks
      `(tree-sitter-hl-face:punctuation.bracket :inherit nil)
      `(tree-sitter-hl-face:punctuation.delimiter :inherit nil)
      `(tree-sitter-hl-face:punctuation :inherit nil)
      ;; Fix embedded code in strings to be distinguishable
      `(tree-sitter-hl-face:embedded :weight bold)))
