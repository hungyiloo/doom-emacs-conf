;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "4d7e8e4d3be7aaff56730f76a066db2acad65371")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "4d7e8e4d3be7aaff56730f76a066db2acad65371")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "7480f020e57036ef14c2fda1d83c830583b2a53b")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "7480f020e57036ef14c2fda1d83c830583b2a53b")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "5126ba6244e13e3e2cf608e7f3955377bcbd8c04")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "093f7e96a323179ee2a68a5a674e7fa2c5d721b8")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "4a0f5405798cfcb98ea005078ef2e2d490e922c4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "87ab7e47e343285f7afd42779c78551332b8fd84")
(package! wgrep)
