;;; config/doom-modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

  ;; I want to see the workspace-name in magit
  (doom-modeline-def-modeline 'vcs
    '(bar workspace-name window-number modals matches buffer-info buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process)))
