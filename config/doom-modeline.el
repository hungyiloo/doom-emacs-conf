;;; config/doom-modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline

  ;; I want to see the workspace-name everywhere
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'minimal
    '(bar workspace-name matches buffer-info-simple)
    '(media-info major-mode))

  (doom-modeline-def-modeline 'special
    '(bar workspace-name window-number modals matches buffer-info buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'project
    '(bar workspace-name window-number buffer-default-directory)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'dashboard
    '(bar workspace-name window-number buffer-default-directory-simple)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'vcs
    '(bar workspace-name window-number modals matches buffer-info buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'package
    '(bar workspace-name window-number package)
    '(misc-info major-mode process))

  (doom-modeline-def-modeline 'info
    '(bar workspace-name window-number buffer-info info-nodes buffer-position parrot selection-info)
    '(misc-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(bar workspace-name window-number buffer-size buffer-info)
    '(misc-info media-info major-mode process vcs))

  (doom-modeline-def-modeline 'message
    '(bar workspace-name window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'pdf
    '(bar workspace-name window-number matches buffer-info pdf-pages)
    '(misc-info major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(bar workspace-name window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker))

  (doom-modeline-def-modeline 'timemachine
    '(bar workspace-name window-number matches git-timemachine buffer-position word-count parrot selection-info)
    '(misc-info minor-modes indent-info buffer-encoding major-mode)))
