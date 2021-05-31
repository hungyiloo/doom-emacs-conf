;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "961318ea22b389ddb47a5d8c391ee7204b106071")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "961318ea22b389ddb47a5d8c391ee7204b106071")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "ca8e82e6c2cc4e5dacff70b9f4223e98e5ecd9af")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "ca8e82e6c2cc4e5dacff70b9f4223e98e5ecd9af")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "94fc7f0f3e687b4b5c5753f8a214274ffaca9459")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "a19bbe94de492bf504399c093cfc5695eb630fa8")
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
