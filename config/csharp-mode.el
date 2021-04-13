;;; config/csharp-mode.el -*- lexical-binding: t; -*-

(after! csharp-mode
  (map! :map csharp-mode-map
        ;; HACK: https://github.com/emacs-csharp/csharp-mode/issues/108
        ;; Fixes some indentation issues that I have, as explained in the link.
        ;; Don't know why I still have them even though those issues were closed in 2017.
        :i "{" #'c-electric-brace))
