;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "4bb320a")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "5daee9e")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "5daee9e")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "4bb320a")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "c93b1b3")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "21cee86")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "b6da466")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "b6da466")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "9d5b95f")
(package! wgrep)
