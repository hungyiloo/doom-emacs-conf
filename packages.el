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
;; (package! eyebrowse)

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

(package! lsp-mode :pin "0f50dec790154e1d003024f87058991c1f24e83e")
(package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
(package! org-roam :pin "1795039ab93ef19611dbb3fca21c4211c4e655a9")
(package! ctrlf :pin "b78e129a8a4fabfebba8cdd5ef51278d0d57e0f4")
(package! org-mode :pin "d70f2806788dce06871287ef02c88ee08076dffc")
(package! vterm :pin "2681120b770573044832ba8c22ccbac192e1a294")
;; (package! good-scroll :pin "fb01f121c4c77db3e6750303894d57b31e410b14")
;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! doom-themes :pin "55f01ed")
(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "d21277a638827623ab84e9e6341312a2da5062ab")
(package! vertico-mouse
  :recipe (:host github :repo "minad/vertico")
  :pin "81a4b35f8d11dfad56de1727ee9bdd3b4461d07c")
(package! literate-calc-mode)

(package! tsx-mode
  :recipe (:local-repo "lisp"))
