;; -*- no-byte-compile: t; -*-

;; Disable evil-escape because it *may* have performance issues.
;; It does show up a lot in the profiler when typing quickly.
;; I don't use it anyway, and my muscle memory always goes for the ESC key.
(package! evil-escape :disable t)

;; simpler than doom's zen
(package! olivetti)

;; semantic syntax highlighting
(package! tree-sitter
  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :pin "2acca5c8d2e3dc66d4d0a99831b33140b5a5f973")
(package! tree-sitter-langs
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :pin "2b845a70080c0edd66f13200b9dc8d6d0c3f42ce")

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

(package! lsp-mode :pin "1f1a97331da472627dd08bfe3e463cfbc5fa0f0a")
(package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
(package! org-roam :pin "17d8e84ea57d4ca27dac999c661a03653cc92a80")
(package! ctrlf :pin "b8a7899faf9d37f1990dfefd9c6b2998c40d7fcc")
(package! org-mode :pin "f33b92dea77f6679b4a5978218989dc7d6a0ed7d")
(package! vterm :pin "2681120b770573044832ba8c22ccbac192e1a294")
(package! magit :pin "2049fd6f6eae7e958b673e809299bc7d3f02a781")
(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "1492aefc00abc3355bf04c2ed05f40ff2f523fcf")
(package! vertico-mouse
  :recipe (:host github :repo "minad/vertico")
  :pin "0df75c0bbc545b1bd008718b1af2e6c0df18fe74")
;; (package! corfu
;;   :recipe (:host github :repo "minad/corfu")
;;   :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! weblorg)

(package! tsx-mode
  :recipe (:local-repo "lisp"))
