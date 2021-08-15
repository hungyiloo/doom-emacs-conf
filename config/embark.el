;;; config/embark.el -*- lexical-binding: t; -*-

(use-package! embark
  :defer t

  :config
  ;; Map the alternative embark-act keybinding to cycle when embark already
  ;; activated, just like the SPC a binding
  (map! :map embark-general-map
        "C-;" #'embark-cycle))
