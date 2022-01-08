;;; config/beacon.el -*- lexical-binding: t; -*-

(add-hook! 'doom-first-buffer-hook
  (beacon-mode +1)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-point-moves-vertically 10))
