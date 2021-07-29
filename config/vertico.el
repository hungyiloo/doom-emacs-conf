;;; config/vertico.el -*- lexical-binding: t; -*-

(after! vertico
  (setq vertico-cycle nil)
  (map! :map vertico-map
       "C-d" (cmd! (vertico-next vertico-count))
       "C-u" (cmd! (vertico-previous vertico-count))
       "C-k" #'kill-line))

(use-package! vertico-mouse
  :after vertico
  :config
  (vertico-mouse-mode +1))
