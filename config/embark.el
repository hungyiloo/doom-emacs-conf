;;; config/embark.el -*- lexical-binding: t; -*-

(use-package! embark
  :defer t

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Map the alternative embark-act keybinding to cycle when embark already
  ;; activated, just like the SPC a binding
  (map! :map embark-general-map
        "C-;" #'embark-cycle))

;; Helpful tries to reset `prefix-help-command', so we set it back
(after! helpful
  (setq prefix-help-command #'embark-prefix-help-command))
