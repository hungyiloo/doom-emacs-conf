;;; config/consult.el -*- lexical-binding: t; -*-

(use-package! consult
  :config
  (defun my/consult-line-dwim ()
    "Conduct a text search on the current buffer.
If a selection is active, pre-fill the prompt with it."
    (interactive)
    (if (region-active-p)
        (consult-line (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-line)))

  (defun my/consult-line-symbol-at-point ()
    "Conduct a text search on the current buffer for the symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun my/consult-ripgrep-dwim (dir)
    "Conduct a text search on in the current (project) directory.
If a selection is active, pre-fill the prompt with it."
    (interactive "P")
    (if (region-active-p)
        (consult-ripgrep dir (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-ripgrep dir)))

  (defun my/consult-ripgrep-symbol-at-point (dir)
    "Conduct a text search on in the current (project) directory for the symbol at point."
    (interactive "P")
    (consult-ripgrep dir (thing-at-point 'symbol)))

  (after! evil
    (evil-set-command-property #'my/consult-line-dwim :jump t))

  ;; Adjust some keybindings to use consult equivalents
  (map! :leader
        "*" #'my/consult-ripgrep-symbol-at-point
        (:prefix-map ("M" . "mode")
         "M" #'consult-mode-command
         "N" #'consult-minor-mode-menu)
        (:prefix-map ("s" . "search")
         "i" #'consult-imenu
         "I" #'consult-outline
         "s" #'my/consult-line-dwim
         "S" #'my/consult-line-thing-at-point
         "b" #'my/consult-line-dwim
         "p" #'my/consult-ripgrep-dwim)
        (:prefix-map ("f" . "file")
         "r" #'consult-recent-file)))
