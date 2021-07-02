;; -*- no-byte-compile: t; -*-

;; Disable evil-escape because it *may* have performance issues.
;; It does show up a lot in the profiler when typing quickly.
;; I don't use it anyway, and my muscle memory always goes for the ESC key.
(package! evil-escape :disable t)

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

;; figure out what mysterious commands are being run
(package! command-log-mode)

;; some sugar for viewing CSV files
(package! csv-mode)

;; window positioning sorcery
(package! transpose-frame)

;; overlay helpers, currently used for rainbow-mode enhancements
(package! ov)

;; rainbow-mode without the extra rgb module stuff (e.g. kurecolor)
(package! rainbow-mode)

(package! lsp-mode :pin "192cbd69d19589299ed6483418b4bc7f0c10e598")
(package! lsp-ui :pin "240a7de26400cf8b13312c3f9acf7ce653bdaa8a")
(package! org-roam :pin "756f6215b672e267f986a3d6e494f5309825b91a")
(package! ctrlf :pin "dbe83710d06bc39315f1455f6f21479f3747c0aa")
(package! org-mode :pin "f76d4d103fe6d8bb61a08d338a1630120f2b255c")
(package! vterm :pin "d9dfa624679afdd5db6ad25429ef86d3dd91401e")
;; (package! good-scroll :pin "fb01f121c4c77db3e6750303894d57b31e410b14")
;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! doom-themes :pin "55f01ed")

(package! tsx-mode
  :recipe (:local-repo "lisp"))
