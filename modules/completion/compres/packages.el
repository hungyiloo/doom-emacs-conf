;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "acbe1cba548832d295449da348719f69b9685c6f")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "acbe1cba548832d295449da348719f69b9685c6f")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "6ce9aa58e74da92e391cfe938bc3da4f47ab591e")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "6ce9aa58e74da92e391cfe938bc3da4f47ab591e")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "3bf0a4db55f6267467f0a08715f4776509a3b503")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "a19bbe94de492bf504399c093cfc5695eb630fa8")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "2646dad28c0819fbe9ee521d39efb9ae40e03982")
(package! wgrep)
