;;; config/ledger.el -*- lexical-binding: t; -*-

(after! ledger-mode
  (add-hook! 'ledger-mode-hook
    (spell-fu-mode -1))
  (map! :map ledger-mode-map
        :nv "[p" #'ledger-navigate-previous-uncleared
        :nv "]p" #'ledger-navigate-next-uncleared
        :nv "SPC m SPC" #'ledger-toggle-current))
