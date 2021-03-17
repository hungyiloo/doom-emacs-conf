;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :hook (doom-first-buffer . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;; This enables tsx tree-sitter features but highlighting doesn't work.
  ;; Currently it's expected to produce this error when visiting a tsx file:
  ;;   tree-sitter-after-on-hook: (wrong-type-argument user-ptrp nil)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-hook! 'typescript-tsx-mode
    (tree-sitter-require 'tsx))
  ;; Temporarily disable tree-sitter-hl-mode on typescript-tsx-mode, because of the above
  (advice-add #'tree-sitter-hl-mode
              :around
              (defun my/tree-sitter-hl-disable-tsx (orig-fun &rest args)
                (unless (eq major-mode 'typescript-tsx-mode)
                  (apply orig-fun args))))

  ;; NOTE: need tree-sitter-cli for this to work
  ;; install via "yarn global add tree-sitter-cli"
  ;; (setq tree-sitter-langs-git-dir (doom-path (getenv "HOME") ".config/emacs/.local/straight/repos/tree-sitter-langs/"))
  )
