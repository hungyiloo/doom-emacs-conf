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
         :desc "Locate file" "F" #'consult-locate))

  ;; Make occur mode (with consult) act more similarly
  ;; to wgrep mode
  (map! :map occur-mode-map
        :n "C-x C-p" nil
        :n "C-c C-p" #'occur-edit-mode)
  (map! :map occur-edit-mode-map
        :n "C-x C-q" nil
        :n "C-c C-k" #'occur-cease-edit
        :n "Z Z" #'occur-cease-edit))
