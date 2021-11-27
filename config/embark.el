;;; config/embark.el -*- lexical-binding: t; -*-

(use-package! embark
  :defer t

  :config
  ;; Map the alternative embark-act keybinding to cycle when embark already
  ;; activated, just like the SPC a binding
  (map! :map embark-general-map
        "C-;" #'embark-cycle)

  (map! :map embark-prose-map
        "`" #'titlecase-region)

  (map! :map embark-region-map
        "`" #'titlecase-region)

  (add-to-list 'embark-pre-action-hooks '(titlecase-region embark--mark-target)))
