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
  :pin "3121b34e207222b2db6ac96a655d68c0edf1a449")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "3121b34e207222b2db6ac96a655d68c0edf1a449")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "6caf9f03972e99c0096a88ed1fc56cd91ebab975")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "6cfd2c2d2744958e695754d6273a8eab2c3fd99c")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "bf0ddeb0b687e6af50ad82558bd32c17a2c0311b")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "bf0ddeb0b687e6af50ad82558bd32c17a2c0311b")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "44935d8962be5724d8a3a4358ce0a4222450ee26")
(package! wgrep)
