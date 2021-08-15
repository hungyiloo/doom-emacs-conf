;;; config/better-jumper.el -*- lexical-binding: t; -*-

(after! (:and evil better-jumper)
  (add-hook! 'find-file-hook
             (evil-set-jump)))
