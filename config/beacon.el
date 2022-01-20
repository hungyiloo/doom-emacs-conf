;;; config/beacon.el -*- lexical-binding: t; -*-

(use-package! beacon
  :init
  (beacon-mode +1)

  :config
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 10)

  (add-to-list 'beacon-dont-blink-major-modes 'vterm-mode)
  (add-to-list 'beacon-dont-blink-major-modes '+doom-dashboard-mode))
