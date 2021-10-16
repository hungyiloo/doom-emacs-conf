;;; config/ledger.el -*- lexical-binding: t; -*-

(after! ledger-mode
  (add-hook! 'ledger-mode-hook
    (spell-fu-mode -1)))
