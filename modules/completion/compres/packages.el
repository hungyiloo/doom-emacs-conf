;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "2d85252ccc18e07c83e0298b0802350bb696efef")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "2d85252ccc18e07c83e0298b0802350bb696efef")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "3d1595e1565b0fa591655f6d1a0a9dd25cd8c984")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "3d1595e1565b0fa591655f6d1a0a9dd25cd8c984")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "e54aa0c4974905cc4da114c3bbcfb084486aa6e1")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "a72109ab3eb3a06d1d8e01629bed06871b2c94d2")
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
