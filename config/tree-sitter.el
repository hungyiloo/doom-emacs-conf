;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :hook (doom-first-buffer . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; Uncomment this section to use CSS tree-sitter highlighting for scss.
  ;; It works OK, but doesn't work for regular double slash comments.
  ;; (pushnew! tree-sitter-major-mode-language-alist
  ;;           '(scss-mode . css))
  )
