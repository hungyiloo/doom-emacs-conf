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
