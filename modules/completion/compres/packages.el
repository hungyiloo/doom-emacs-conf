;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "f52f9a34893a0dda493e22326aa859545a56bfe8")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "f52f9a34893a0dda493e22326aa859545a56bfe8")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "6020efd7de9882fca0b67c80876454e1994b72e9")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "6020efd7de9882fca0b67c80876454e1994b72e9")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "c940a49dbda97b078c240b7a7a076588e2adb30f")
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
