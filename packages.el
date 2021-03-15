;; -*- no-byte-compile: t; -*-

;; Disable evil-escape because it *may* have performance issues.
;; It does show up a lot in the profiler when typing quickly.
;; I don't use it anyway, and my muscle memory always goes for the ESC key.
(package! evil-escape :disable t)

;; Kurecolor doesn't work well for me
(package! kurecolor :disable t)

;; simpler than doom's zen
(package! olivetti)

;; semantic syntax highlighting
(package! tree-sitter)
(package! tree-sitter-langs)

;; chinese pinyin input
(package! pyim)

;; simpler and more robust than persp-mode and doom's workspaces, but fewer features
(package! eyebrowse)

;; org-babel typescript support
(package! ob-typescript)

;; smooth scrolling when jump-navigating
;; disabled because it doesn't work on some of my systems (pgtk?)
;; and also because it interferes with evil/better jumper
;; (package! scroll-on-jump
;;   :recipe (:host gitlab :repo "ideasman42/emacs-scroll-on-jump")
;;   :pin "69c86542")

;; figure out what mysterious commands are being run
(package! command-log-mode)

;; some sugar for viewing CSV files
(package! csv-mode)

;; window positioning sorcery
(package! transpose-frame)

;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! lsp-mode :pin "d5c7f7e")
;; (package! doom-themes :pin "55f01ed")
