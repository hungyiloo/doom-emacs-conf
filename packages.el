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

(package! lsp-mode :pin "32d34445f3c4da37840c0ebb3a12e2c0627782a8")
(package! lsp-ui :pin "177c31e982345ba35dc7c5d90cb1f8e68585323a")
;; (package! org-roam :pin "756f6215b672e267f986a3d6e494f5309825b91a")
(package! ctrlf :pin "b78e129a8a4fabfebba8cdd5ef51278d0d57e0f4")
(package! org-mode :pin "09cf4a075efe62262cb103605c5bd9c6f5cdcae2")
(package! vterm :pin "f73644a07ce56712101d1568139a8f3c3b4a9e59")
;; (package! good-scroll :pin "fb01f121c4c77db3e6750303894d57b31e410b14")
;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! doom-themes :pin "55f01ed")
(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "fcd70f55918e0f015418cb00c20b04d282cb211b")
(package! vertico-mouse
  :recipe (:host github :repo "minad/vertico")
  :pin "d00ad0697024aa9555972ea2bce678080272d633")

(package! tsx-mode
  :recipe (:local-repo "lisp"))
