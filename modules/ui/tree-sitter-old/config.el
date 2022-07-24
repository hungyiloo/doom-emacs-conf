;;; ui/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                               (error-message-string e))
         (signal (car e) (cadr e))))))

  (defadvice! doom-tree-sitter-point-in-comment-p-override (orig-fun &rest args)
              :around #'doom-point-in-comment-p
              ;; Use tree-sitter instead of smartparens to detect if we're in a comment
              (apply (if tree-sitter-mode
                         #'doom-tree-sitter-point-in-comment-p
                       orig-fun)
                     args))

  (after! tree-sitter-hl
    (custom-set-faces!
      ;; Fix blotches of wrong background color in org src blocks
      `(tree-sitter-hl-face:punctuation.bracket :inherit nil)
      `(tree-sitter-hl-face:punctuation.delimiter :inherit nil)
      `(tree-sitter-hl-face:punctuation :inherit nil)
      ;; Fix embedded code in strings to be distinguishable
      `(tree-sitter-hl-face:embedded :weight bold))))
