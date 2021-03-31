;;; config/css-mode.el -*- lexical-binding: t; -*-

(after! css-mode
  ;; Use 2-space indentation in css
  (setq css-indent-offset 2)
  ;; I use a modified rainbow-mode for colorizing
  (setq css-fontify-colors nil))
