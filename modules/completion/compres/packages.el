;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "1edbb1af2a6ff2f25a34d9c769baf6fc175a7ef7")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "1edbb1af2a6ff2f25a34d9c769baf6fc175a7ef7")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "bd58f2e3b7a9bd52145aa97997e62ec5d9e7e4d5")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "bd58f2e3b7a9bd52145aa97997e62ec5d9e7e4d5")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "e54aa0c4974905cc4da114c3bbcfb084486aa6e1")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "a8806f71f9cc07daa0149c89a4dbdae0aa5aebff")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "52afa7e90534d59d3cec2ace2a96c232e25e3f7b")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "52afa7e90534d59d3cec2ace2a96c232e25e3f7b")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "9d5b95f40275dc47a57e9d4ee9b9994ee3a4b426")
(package! wgrep)
