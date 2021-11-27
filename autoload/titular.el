;;; titular.el --- convert text to title case -*- lexical-binding: t; -*-

;;;###autoload (autoload #'my/evil-titlecase-operator "autoload/titular" nil t)
(evil-define-operator my/evil-titlecase-operator (beg end)
  (interactive "<r>")
  (save-excursion
    (set-mark beg)
    (goto-char end)
    (titlecase-dwim)))

