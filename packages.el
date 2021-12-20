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

(package! lsp-mode :pin "e43b9ddb3903a39c325966f21e7edc1192d1e8e2")
(package! lsp-ui :pin "98d0ad00b8bf1d3a7cea490002169f2286d7208c")
(package! org-roam :pin "abe63b436035049923ae96639b9b856697047779")
(package! ctrlf :pin "e915c5920cd3e39f481a6ce024073dd28cc9f743")
(package! vterm :pin "ed6e867cfab77c5a311a516d20af44f57526cfdc")
(package! magit :pin "1eb183e7672bf25fa77ea06d97b3d9c502a698ae")
(package! embark :recipe (:host github :repo "oantolin/embark") :pin "33b47bb8999f8c7d67e9d1e04668e421e3c7246f")
(package! vertico-mouse :recipe (:host github :repo "minad/vertico") :pin "eedcb847869226701acaf9a36dce0a51d1b60862")
;; (package! corfu :recipe (:host github :repo "minad/corfu") :pin "68bba8355b0d84199f1b55442f9447d9b9c70bea")
(package! literate-calc-mode)
(package! weblorg)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

(package! tsx-mode
  :recipe (:local-repo "lisp"))
(package! charge
  :recipe (:local-repo "lisp"))
(package! titular
  :recipe (:host github :repo "hungyiloo/titular.el") :pin "688dff7f7dc945e2fe71f743cfee3f9f032b4b83")
