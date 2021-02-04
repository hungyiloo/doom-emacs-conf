(after! tree-sitter
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; Uncomment this section to use CSS tree-sitter highlighting for scss.
  ;; It works OK, but doesn't work for regular double slash comments.
  ;; (pushnew! tree-sitter-major-mode-language-alist
  ;;           '(scss-mode . css))
  )
