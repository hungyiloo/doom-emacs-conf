;;; config/vterm.el -*- lexical-binding: t; -*-

(after! vterm
  ;; Start vterm in normal mode always
  (evil-set-initial-state 'vterm-mode 'normal))
