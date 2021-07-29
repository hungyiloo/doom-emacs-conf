;;; config/orderless.el -*- lexical-binding: t; -*-

(use-package! orderless
  :defer t
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-style-dispatchers `(,(defun my/orderless-without-if-at-bang-dispatcher (pattern _index _total)
                                         (when (string-prefix-p "@!" pattern)
                                           `(orderless-without-literal . ,(substring pattern 2))))))
  (custom-set-faces!
    `(orderless-match-face-0 :foreground ,(doom-color 'magenta) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-1 :foreground ,(doom-color 'yellow) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-2 :foreground ,(doom-color 'cyan) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-3 :foreground ,(doom-color 'green) :bold t :background ,(doom-color 'base0))))
