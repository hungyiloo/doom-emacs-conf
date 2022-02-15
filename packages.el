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

(package! lsp-mode :pin "9b3a9215807af0727b514e8c7cf440bcc0bdad44")
(package! lsp-ui :pin "21ce926eedd41ef305c2d89412506ce59b1a7eac")
(package! org-roam :pin "b163c900b8ec2e3637bc251d9b90421efc771c02")
(package! vterm :pin "a940dd2ee8a82684860e320c0f6d5e15d31d916f")
(package! magit :pin "edc593c7c112dc6c26244acdd8755900175eaf67")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "bc405f0e48a358dcf4ac1d4a20335f5060e3ab76")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "c5d121d60d7f7fdb995c1b15c9e4ba3c3052a0ad")
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
