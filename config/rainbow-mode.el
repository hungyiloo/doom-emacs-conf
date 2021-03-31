;;; config/rainbow-mode.el -*- lexical-binding: t; -*-

(after! rainbow-mode
  (setq rainbow-html-colors-major-mode-list '(html-mode css-mode scss-mode php-mode nxml-mode xml-mode))
  ;; Enhance rainbow-mode to use overlays instead of text properties.
  ;; This prevents inteferefence with hl-line-mode.
  ;; Source: https://github.com/amosbird/rainbow-mode
  (advice-add #'rainbow-colorize-match :around #'my/rainbow-colorize-match-override)
  (advice-add #'rainbow-turn-off :around #'my/rainbow-turn-off-override))
