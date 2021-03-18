;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "4e3e751725787f18b55defc2fe7d8115adf524fd")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "4e3e751725787f18b55defc2fe7d8115adf524fd")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "3d1595e1565b0fa591655f6d1a0a9dd25cd8c984")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "3d1595e1565b0fa591655f6d1a0a9dd25cd8c984")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "e741b243b30f6cfe85e568cc551acff9a1e5e74f")
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
