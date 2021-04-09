;;; config/csv-mode.el -*- lexical-binding: t; -*-

(add-hook! 'csv-mode-hook
  (visual-line-mode -1)
  (spell-fu-mode -1))
