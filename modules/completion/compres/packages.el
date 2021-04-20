;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "b8f82c9b6bdcd6e4d53e7d0bad869f3c8a8ef2f9")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "b8f82c9b6bdcd6e4d53e7d0bad869f3c8a8ef2f9")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "d321642973c6b62947d004bd6b3f387a84c5c131")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "d321642973c6b62947d004bd6b3f387a84c5c131")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "9389f38a2d1366f1b70d33a60ccc6f3926358b60")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "2009e5490034855d151b8ac0fa5af73c61c6e74f")
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
