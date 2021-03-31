;;; config/hydra.el -*- lexical-binding: t; -*-

(after! hydra
  ;; Fix emojification of emoji in hydra hints.
  (advice-add #'hydra-show-hint :around #'my/emojify-hydra-hint))
