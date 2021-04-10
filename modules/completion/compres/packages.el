;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "5f3097824f8c3d17bcd70c4e4ce597bcfcf2196f")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "5f3097824f8c3d17bcd70c4e4ce597bcfcf2196f")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "846c715ee1df94292a9bb2467810bd7959ccf078")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "846c715ee1df94292a9bb2467810bd7959ccf078")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "668265af921285c726b2239dae32459bd1064d03")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "9c5f142107d28868748a5801fb53ca7c5ad75fec")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "bf0ddeb0b687e6af50ad82558bd32c17a2c0311b")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "bf0ddeb0b687e6af50ad82558bd32c17a2c0311b")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "87ab7e47e343285f7afd42779c78551332b8fd84")
(package! wgrep)
