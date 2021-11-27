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

(package! lsp-mode :pin "3a960e8b0575a6e4ba4bf4d547d2825de645a20c")
(package! lsp-ui :pin "dd4c181a22d19a28236c442cf6c9cd4bbd6d85f8")
(package! org-roam :pin "67f10864df04e6a789dcca03e8cf7637b571098b")
(package! ctrlf :pin "e915c5920cd3e39f481a6ce024073dd28cc9f743")
(package! vterm :pin "2681120b770573044832ba8c22ccbac192e1a294")
(package! magit :pin "a66b86d51139479d74962cc077c5215d20dd72e6")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "e0057f6a6621b24e0ba4c167b683a84039c6f084")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "f3f55e4f5923fe2108a59eedb68e9989d1cef151")
;; (package! corfu :recipe (:host github :repo "minad/corfu") :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! weblorg)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! tsx-mode
  :recipe (:local-repo "lisp"))
(package! charge
  :recipe (:local-repo "lisp"))
(package! titular
  :recipe (:local-repo "lisp"))
