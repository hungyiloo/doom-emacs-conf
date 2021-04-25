;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "4d7e8e4d3be7aaff56730f76a066db2acad65371")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "4d7e8e4d3be7aaff56730f76a066db2acad65371")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "61f8a7f19211b38d3600d99e494478c9f347803f")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "61f8a7f19211b38d3600d99e494478c9f347803f")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "5159256d04d123899b88ee6e7eba0c27f66d0fe2")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "093f7e96a323179ee2a68a5a674e7fa2c5d721b8")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "ed2b762241bbea03e374dc9dcd4fbe207c6b2ea4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "ed2b762241bbea03e374dc9dcd4fbe207c6b2ea4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "87ab7e47e343285f7afd42779c78551332b8fd84")
(package! wgrep)
