;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "3509503fa59c69f9c8545f27fb850c5ea1bd93b1")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "3509503fa59c69f9c8545f27fb850c5ea1bd93b1")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "fff31b0f3d890fa380e797585f3d7c4f3541b429")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "fff31b0f3d890fa380e797585f3d7c4f3541b429")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "39f2ef17a6ab838ae4c2380bc24b496cb72aa145")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "97693d0aea2c548197e9d1de3bdedf8e703775a4")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "027c2137a8d9e01a1d4c7b5e5d98da017dd2d48e")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "027c2137a8d9e01a1d4c7b5e5d98da017dd2d48e")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "1e84120a28525ccb47b602fc19b7afbeffbbe502")
(package! wgrep)
