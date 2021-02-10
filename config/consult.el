;;; config/consult.el -*- lexical-binding: t; -*-

(use-package! consult
  :config
  (defun my-consult-line-dwim ()
    "Conduct a text search on the current buffer.
If a selection is active, pre-fill the prompt with it."
    (interactive)
    (if (region-active-p)
        (consult-line (rxt-pcre-to-elisp (rxt-quote-pcre (buffer-substring-no-properties (region-beginning) (region-end)))))
      (consult-line)))
  (after! evil
    (evil-set-command-property #'my-consult-line-dwim :jump t))

  ;; Adjust some keybindings to use consult equivalents
  (map! :leader
        (:prefix-map ("M" . "mode")
         "M" #'consult-mode-command
         "N" #'consult-minor-mode-menu)
        (:prefix-map ("s" . "search")
         "i" #'consult-imenu
         "I" #'consult-outline
         "s" #'my-consult-line-dwim
         "p" #'consult-ripgrep)
        (:prefix-map ("f" . "file")
         "r" #'consult-recent-file)))
