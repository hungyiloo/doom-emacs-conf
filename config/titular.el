;;; config/titular.el -*- lexical-binding: t; -*-

(after! evil
  (map! :nv "g`" #'my/evil-titlecase-operator))
