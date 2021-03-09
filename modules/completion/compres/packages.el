;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "38598d2eeca97f79dad266d4bb07eb830da1fe8c")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "38598d2eeca97f79dad266d4bb07eb830da1fe8c")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "5daee9e")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "5daee9e")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "153417adc9d7779a5393ad37c6584f875291b6ac")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "21cee86")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "84b0918")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "84b0918")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "9d5b95f")
(package! wgrep)
