;;; config/beacon.el -*- lexical-binding: t; -*-

(use-package! beacon
  :init
  (beacon-mode +1)

  :config
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-point-moves-vertically 10)

  (after! evil
    (defun my/beacon-blink-advice (&rest ignored) (beacon-blink))
    (advice-add #'evil-scroll-up :after #'my/beacon-blink-advice)
    (advice-add #'evil-scroll-down :after #'my/beacon-blink-advice)
    (advice-add #'evil-scroll-page-up :after #'my/beacon-blink-advice)
    (advice-add #'evil-scroll-page-down :after #'my/beacon-blink-advice)))
