;;; config/titular.el -*- lexical-binding: t; -*-
(use-package! titular
  :commands (titlecase-region titlecase-dwim))

(after! evil
  (map! :nv "g`" #'my/evil-titlecase-operator))
