;;; config/tree-sitter.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :hook (doom-first-buffer . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;; This enables tsx tree-sitter features but highlighting doesn't work.
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx))
  (add-hook! '(typescript-tsx-mode tsx-mode)
    (tree-sitter-require 'tsx))
  ;; Temporarily disable tree-sitter-hl-mode on typescript-tsx-mode, because of the above
  ;; (advice-remove #'tree-sitter-hl-mode #'my/tree-sitter-hl-disable-tsx)
  (advice-add #'tree-sitter-hl-mode
              :around
              (defun my/tree-sitter-hl-disable-tsx (orig-fun &rest args)
                (unless (eq major-mode 'typescript-tsx-mode)
                  (apply orig-fun args))))
  ;; (advice-add #'font-lock-eval-keywords
  ;;             :around
  ;;             (defun my/font-lock-eval-keywords-quote-safe (orig-fun keywords)
  ;;               (unless (or (not keywords) (eq keywords 'quote))
  ;;                   (apply orig-fun (list keywords)))))

  ;; NOTE: need tree-sitter-cli for this to work
  ;; install via "yarn global add tree-sitter-cli"
  ;; (setq tree-sitter-langs-git-dir (doom-path (getenv "HOME") ".config/emacs/.local/straight/repos/tree-sitter-langs/"))

  (custom-set-faces!
    ;; Fix blotches of wrong background color in org src blocks
    `(tree-sitter-hl-face:punctuation :inherit nil)))
