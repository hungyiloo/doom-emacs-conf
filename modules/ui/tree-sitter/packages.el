;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :pin "2acca5c8d2e3dc66d4d0a99831b33140b5a5f973")
(package! tree-sitter-langs
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :pin "2b845a70080c0edd66f13200b9dc8d6d0c3f42ce")
