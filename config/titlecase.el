;;; config/titlecase.el -*- lexical-binding: t; -*-

(use-package! titlecase
  :commands (titlecase-dwim titlecase-region)
  :load-path "lisp"
  :config
  (setq titlecase-command (concat doom-private-dir "bin/titlecase")))
