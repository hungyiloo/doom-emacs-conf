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

(package! lsp-mode :pin "c6223e9f8d4b2566cd3f9153ffed67a2ca10b34f")
(package! lsp-ui :pin "cb02972b20706d18d137841c83d3765bcb280687")
(package! org-roam :pin "8ad141403065bebd5a72f0ef53cf5ef8f2034419")
(package! ctrlf :pin "dbe83710d06bc39315f1455f6f21479f3747c0aa")
(package! org-mode :pin "a0a87d55cbfea043a55e484e262c48b8fcdfdb47")
(package! vterm :pin "2b1392cb2b14ec5bd0b7355197d5f353aa5d3983")
;; (package! good-scroll :pin "fb01f121c4c77db3e6750303894d57b31e410b14")
;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! doom-themes :pin "55f01ed")

(package! tsx-mode
  :recipe (:local-repo "lisp"))
