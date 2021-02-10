;;; config/pyim.el -*- lexical-binding: t; -*-

(use-package! pyim
  :commands #'set-input-method
  :config
  (require 'pyim-basedict)
  (pyim-basedict-enable))
