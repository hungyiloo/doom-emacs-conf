;;; config/doom.el -*- lexical-binding: t; -*-

(add-hook! 'after-init-hook
  (advice-add #'doom/bump-package-at-point
              :around
              (defun my/doom-bump-package-at-point-prevent-double-pin (orig-fun &rest args)
                "Prevents doom from adding a duplicate pin property when bumping package at point."
                (save-excursion
                  (when (and (not (eq (char-after) 40))
                             (doom--package-at-point))
                    (search-backward "(package!"))
                  (apply orig-fun args)))))
