;;; config/embark.el -*- lexical-binding: t; -*-

(use-package! embark
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Helpful tries to reset `prefix-help-command', so we set it back
(after! helpful
  (setq prefix-help-command #'embark-prefix-help-command))
