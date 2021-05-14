;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "d82f8c73fae4d2d7283838cf5111366384775977")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "d82f8c73fae4d2d7283838cf5111366384775977")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "b873ceeefcb80ae0a00aa5e9ce7d70a71680aa4b")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "b873ceeefcb80ae0a00aa5e9ce7d70a71680aa4b")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "445d2832a2f06484ad28d9b55676c52d63cd0a46")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "bfefb8e1a350d44b56290b2c7ddc3418ec217b30")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "d97a91f6e12ace638e65bdccefd14d1e638a2dae")
(package! wgrep)
