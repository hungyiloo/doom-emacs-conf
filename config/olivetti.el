;;; config/olivetti.el -*- lexical-binding: t; -*-

(use-package! olivetti
  :hook (org-mode . olivetti-mode)
  :hook (markdown-mode . olivetti-mode)
  :hook (olivetti-mode . my/olivetti-setup)
  :init
  (setq olivetti-body-width 80)
  (defun my/olivetti-setup ()
    (setq doom--line-number-style nil)
    (setq display-line-numbers nil)))
