;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :hook (doom-first-buffer . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;; NOTE: need tree-sitter-cli for this to work
  ;; install via "yarn global add tree-sitter-cli"
  ;; (setq tree-sitter-langs-git-dir (doom-path (getenv "HOME") ".config/emacs/.local/straight/repos/tree-sitter-langs/"))
  )
