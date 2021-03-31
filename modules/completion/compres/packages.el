;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "33e9af8403b22f75c01db96f39aee344de6ffaa8")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "33e9af8403b22f75c01db96f39aee344de6ffaa8")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "3b032d17e5a9355821222ed5b732d956b3c65d81")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "3b032d17e5a9355821222ed5b732d956b3c65d81")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "f26374545275cdde96d67576a43f3a919b6927cd")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "847947bb3fdc5991132ab9005feea0c5d650417f")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "8573df977eaceffc6607b7242ff8c0dab02aad65")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "8573df977eaceffc6607b7242ff8c0dab02aad65")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "44935d8962be5724d8a3a4358ce0a4222450ee26")
(package! wgrep)
