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

;; mermaid JS charts
(package! mermaid-mode)

;; shine a light on cursor for big movements
(package! beacon)

(package! lsp-mode :pin "a82a4fa3467ec918273ab65d48c5c7d2dbfaec74")
(package! lsp-ui :pin "21ce926eedd41ef305c2d89412506ce59b1a7eac")
(package! org-roam :pin "679ef6ef001fd1a69b691108178721aa913e7f0f")
(package! vterm :pin "a940dd2ee8a82684860e320c0f6d5e15d31d916f")
(package! magit :pin "a5f6705bf9a0b040a77eba67bafeec51ada90649")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "8cf1fdbfacdbdb98ca3b4e50bf295059a02fe4a2")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "8310587734250814be9b21483253017ffd0f6399")
;; (package! corfu :recipe (:host github :repo "minad/corfu") :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! org-alert)

(package! tsx-mode
  :recipe (:local-repo "lisp"))
(package! charge
  :recipe (:local-repo "lisp"))
(package! titular
  :recipe (:host github :repo "hungyiloo/titular.el"))
