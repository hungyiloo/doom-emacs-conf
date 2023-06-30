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
(package! ob-mermaid)

(package! org-roam :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! literate-calc-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! org-alert)

(package! tsx-mode
  :recipe (:local-repo "lisp"))
(package! charge
  :recipe (:local-repo "lisp"))
(package! tarzan
  :recipe (:local-repo "lisp"))
(package! titular
  :recipe (:host github :repo "hungyiloo/titular.el"))
