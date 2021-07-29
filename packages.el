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
(package! eyebrowse)

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

(package! lsp-mode :pin "b4e51450188acf41f8273542db299936461087a4")
(package! lsp-ui :pin "4283414de69312298d51b03e938d95d37d238391")
;; (package! org-roam :pin "756f6215b672e267f986a3d6e494f5309825b91a")
(package! ctrlf :pin "b78e129a8a4fabfebba8cdd5ef51278d0d57e0f4")
(package! org-mode :pin "c9dfed48a607c7f6524f1c6480f09cf61a5d6237")
(package! vterm :pin "d9dfa624679afdd5db6ad25429ef86d3dd91401e")
;; (package! good-scroll :pin "fb01f121c4c77db3e6750303894d57b31e410b14")
;; (package! evil :pin "f5ab7ff")
;; (package! evil-mc :pin "f04fb17")
;; (package! doom-themes :pin "55f01ed")
(package! vertico-mouse
  :recipe (:host github :repo "minad/vertico")
  :pin "51152475cf96c78a795525675999f6d12545c0ec")

(package! tsx-mode
  :recipe (:local-repo "lisp"))
