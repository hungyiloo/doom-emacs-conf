;;; config/vertico.el -*- lexical-binding: t; -*-

(after! vertico
  (setq vertico-cycle nil)
  (map! :map vertico-map
       "C-d" #'vertico-scroll-up ;; these are reversed for some reason?!
       "C-u" #'vertico-scroll-down
       "C-k" #'kill-line))

(use-package! vertico-mouse
  :after vertico
  :config
  (vertico-mouse-mode +1))
