;;; config/typescript-mode.el -*- lexical-binding: t; -*-

(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq tide-native-json-parsing t)
  (setq tide-completion-ignore-case t))
