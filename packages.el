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
  :pin "4d9871d23999fe5f8de821e23c9ec576df2b2738")
(package! tree-sitter-langs
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :pin "fa47b55f7bd11bd2b17ab48deb03ed23000bb974")

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

(package! lsp-mode :pin "d5d7a54dee2622d3fd884638617f4957e1876018")
(package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
(package! org-roam :pin "1795039ab93ef19611dbb3fca21c4211c4e655a9")
(package! ctrlf :pin "b8a7899faf9d37f1990dfefd9c6b2998c40d7fcc")
(package! org-mode :pin "a246a131873e566afe243ab98b59a806b31019b8")
(package! vterm :pin "2681120b770573044832ba8c22ccbac192e1a294")
(package! magit :pin "1e40d0021790707f6e88debda04f6b14d9429586")
(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "64c4132c6e29d2e415325a50f190ab8a0732a4e4")
(package! vertico-mouse
  :recipe (:host github :repo "minad/vertico")
  :pin "81a4b35f8d11dfad56de1727ee9bdd3b4461d07c")
(package! literate-calc-mode)

(package! tsx-mode
  :recipe (:local-repo "lisp"))
