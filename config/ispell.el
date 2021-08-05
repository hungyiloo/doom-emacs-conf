;;; config/ispell.el -*- lexical-binding: t; -*-

(after! ispell
  (setq ispell-dictionary "en"))

(after! spell-fu
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:comment)
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:doc)
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:string)
  (add-to-list 'spell-fu-faces-exclude 'tree-sitter-hl-face:embedded))
