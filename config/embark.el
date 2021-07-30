;;; config/embark.el -*- lexical-binding: t; -*-

(use-package! embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command))
