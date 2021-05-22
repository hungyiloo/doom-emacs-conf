;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "a21e510bc63c8ddc98b2bb3e6fff38e9d7f41ca9")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "a21e510bc63c8ddc98b2bb3e6fff38e9d7f41ca9")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "556ff4eb31eb1d00a2afdda6664d03b698264e3c")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "556ff4eb31eb1d00a2afdda6664d03b698264e3c")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "624028c69b55deb3387452b9eeabe9cb963bd2a4")
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
