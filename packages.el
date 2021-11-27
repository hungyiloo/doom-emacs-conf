;; -*- no-byte-compile: t; -*-

;; Disable evil-escape because it *may* have performance issues.
;; It does show up a lot in the profiler when typing quickly.
;; I don't use it anyway, and my muscle memory always goes for the ESC key.
(package! evil-escape :disable t)

;; simpler than doom's zen
(package! olivetti)

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

(package! lsp-mode :pin "e56ed27181a4ec2330c7d61cb5a6c2531820511b")
(package! lsp-ui :pin "dd4c181a22d19a28236c442cf6c9cd4bbd6d85f8")
(package! org-roam :pin "d93423d4e11da95bcf177b2bc3c74cb1d1acf807")
(package! ctrlf :pin "e915c5920cd3e39f481a6ce024073dd28cc9f743")
(package! vterm :pin "2681120b770573044832ba8c22ccbac192e1a294")
(package! magit :pin "f15becef69f4695f76dd8d00b7dd571663dd8a33")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "5182b7eb2200a9245f1e5f73806672d6dcdd0356")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "771335535cde6819baf6904bb7101545c0e01e7a")
;; (package! corfu :recipe (:host github :repo "minad/corfu") :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! weblorg)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! tsx-mode
  :recipe (:local-repo "lisp"))

(package! charge
  :recipe (:local-repo "lisp"))
