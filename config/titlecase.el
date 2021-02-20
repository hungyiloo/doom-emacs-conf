;;; config/titlecase.el -*- lexical-binding: t; -*-

(use-package! titlecase
  :commands (titlecase-dwim titlecase-region)
  :load-path "lisp"
  :config
  (after! evil
    (map! :nv "g`" (evil-define-operator my/evil-titlecase-operator (beg end)
                     (interactive "<r>")
                     (save-excursion
                       (set-mark beg)
                       (goto-char end)
                       (titlecase-dwim))))))
