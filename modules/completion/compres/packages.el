;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "0da967adf0b1c17c59d1c0a1c166c983afe640b2")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "0da967adf0b1c17c59d1c0a1c166c983afe640b2")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "b43dfe24205283b5a13985bdb332898647281d16")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "b43dfe24205283b5a13985bdb332898647281d16")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "3f33b38b7c1ecd7086942e1bd8284c54a6fd30a3")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "a922b19f715ad6d046072a35a3df5ac5e4ed73d3")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "9637d7fd59f76a5b6d37470b1543ab827a0f9b8d")
(package! wgrep)
