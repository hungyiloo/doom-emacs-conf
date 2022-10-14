;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(after! tree-sitter
  (defadvice! doom-tree-sitter-point-in-comment-p-override (orig-fun &rest args)
    :around #'doom-point-in-comment-p
    ;; Use tree-sitter instead of smartparens to detect if we're in a comment
    (apply (if tree-sitter-mode
               #'doom-tree-sitter-point-in-comment-p
             orig-fun)
           args))

  ;; Add tree-sitter support fox my custom tsx-mode
  (add-hook 'tsx-mode-hook #'tree-sitter-hl-mode)
  (setf (map-elt
         evil-textobj-tree-sitter-major-mode-language-alist
         'tsx-mode)
        "typescript")

  ;; Don't disable tree-sitter-hl-mode anywhere
  (setq +tree-sitter-hl-enabled-modes t))
