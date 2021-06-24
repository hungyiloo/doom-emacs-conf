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
  :pin "ee58941308d83a717728f056ea753e80f68cfbc0")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "ee58941308d83a717728f056ea753e80f68cfbc0")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "9796fada769f44cb8e05914bd6be3fcc15d791e2")
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
