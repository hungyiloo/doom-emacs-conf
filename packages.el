;; -*- no-byte-compile: t; -*-

;; Disable evil-escape because it *may* have performance issues.
;; It does show up a lot in the profiler when typing quickly.
;; I don't use it anyway, and my muscle memory always goes for the ESC key.
(package! evil-escape :disable t)

(package! olivetti)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! pyim)
(package! eyebrowse)
(package! ob-typescript)
(package! scroll-on-jump
  :recipe (:host gitlab :repo "ideasman42/emacs-scroll-on-jump"))
(package! command-log-mode)
(package! csv-mode)

;; I always want the latest updates from these doom-included packages
(unpin! evil)
(unpin! evil-mc)
(unpin! lsp-mode)
(unpin! doom-themes)
