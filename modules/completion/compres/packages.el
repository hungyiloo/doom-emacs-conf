;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "e6d9835cbe54dfcb206fd97513772cc4e503af38")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "e6d9835cbe54dfcb206fd97513772cc4e503af38")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "19540d37783dc34bdb98d7cea24e8bb57090dab4")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "19540d37783dc34bdb98d7cea24e8bb57090dab4")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "5ce5a0e6d23d92391167a49d994f093764ee0dee")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "83b296c3c9023009b3ef504c968789e5e726ac55")
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
