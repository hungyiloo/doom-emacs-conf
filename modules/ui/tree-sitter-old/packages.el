;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
(package! tree-sitter-langs
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :pin "599570cd2a6d1b43a109634896b5c52121e155e3")
