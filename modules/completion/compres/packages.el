;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "05aa11bca37db1751c86fe78f784741be5b1a066")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "05aa11bca37db1751c86fe78f784741be5b1a066")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "2e5b280001e7c22204ae538234efc4a107fa423d")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "2e5b280001e7c22204ae538234efc4a107fa423d")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "d1b836db16cb693293a2cb7064e5cf9df625df2a")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "c68c7f6c21877b09734a8543fee363cf2fbbecf4")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "87ab7e47e343285f7afd42779c78551332b8fd84")
(package! wgrep)
