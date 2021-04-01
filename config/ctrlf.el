;;; config/ctrlf.el -*- lexical-binding: t; -*-

(ctrlf-mode 1)
(after! ctrlf
  ;; Clearer active search face
  (custom-set-faces!
    `(ctrlf-highlight-active :background ,(doom-color 'magenta 256) :foreground ,(doom-color 'base0) :weight bold))

  ;; Disable ctrlf in pdf-tools isearch
  (add-hook! 'pdf-isearch-minor-mode-hook
    (ctrlf-local-mode -1)))
