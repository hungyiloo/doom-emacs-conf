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

;; arbitrary file annotations
(package! annotate.el)

(package! lsp-mode :pin "a82a4fa3467ec918273ab65d48c5c7d2dbfaec74")
(package! lsp-ui :pin "21ce926eedd41ef305c2d89412506ce59b1a7eac")
(package! org-roam :pin "679ef6ef001fd1a69b691108178721aa913e7f0f")
(package! ctrlf :pin "282eaa836d2198bb5947dfd3c454ae5305f55efb")
(package! vterm :pin "a940dd2ee8a82684860e320c0f6d5e15d31d916f")
(package! magit :pin "386843483b262ad57b24aec3167035b01acb4bf1")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "c9b26c2e18f01ae401df6a69b7a0c1a6bc44b90c")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "a8fe9a0b2e156e022136169a3159b4dad78b2439")
;; (package! corfu :recipe (:host github :repo "minad/corfu") :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! tsx-mode
  :recipe (:local-repo "lisp"))
(package! charge
  :recipe (:local-repo "lisp"))
(package! titular
  :recipe (:host github :repo "hungyiloo/titular.el") :pin "688dff7f7dc945e2fe71f743cfee3f9f032b4b83")
