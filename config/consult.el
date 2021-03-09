;;; config/consult.el -*- lexical-binding: t; -*-

(use-package! consult
  :config
  ;; Adjust some keybindings to use consult equivalents
  (map! :leader
        "," #'+compres/consult-project-buffer
        "<" #'consult-buffer
        (:prefix-map ("M" . "mode")
         "M" #'consult-mode-command
         "N" #'consult-minor-mode-menu)
        (:prefix-map ("s" . "search")
         "I" #'consult-project-imenu
         "S" #'+compres/consult-line-symbol-at-point
         :desc "Search for file" "f" #'consult-find
         :desc "Locate file" "F" #'consult-locate)))
