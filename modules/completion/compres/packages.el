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
  :pin "7b324c597406b77eb18591bb3ce23b2078eb992e")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "7b324c597406b77eb18591bb3ce23b2078eb992e")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "2d9a40a920a56d1e19b54c9265c0d2c2ad732a64")
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
  :pin "e85084e733d6eb50893974fc5fd569b944a5010c")
(package! wgrep)
