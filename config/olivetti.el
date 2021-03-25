;;; config/olivetti.el -*- lexical-binding: t; -*-

(use-package! olivetti
  :hook (olivetti-mode . my/olivetti-setup)
  :init
  (setq olivetti-body-width 80)
  (defun my/olivetti-setup ()
    (setq doom--line-number-style (not olivetti-mode))
    (setq display-line-numbers (not olivetti-mode))
    (git-gutter-mode (if olivetti-mode -1 1)))
  (map! :leader
        (:prefix-map ("t" . "toggle")
         "z" #'olivetti-mode))
  :config
  (after! org-roam
    (defun my/disable-olivetti-for-roaming (orig-fun &rest args)
      (when olivetti-mode
        (olivetti-mode -1))
      (apply orig-fun args))
    (advice-add #'org-roam-capture :around #'my/disable-olivetti-for-roaming)
    (advice-add #'org-roam-find-file :around #'my/disable-olivetti-for-roaming)))
