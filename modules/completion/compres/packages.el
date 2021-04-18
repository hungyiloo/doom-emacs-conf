;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "b8f82c9b6bdcd6e4d53e7d0bad869f3c8a8ef2f9")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "b8f82c9b6bdcd6e4d53e7d0bad869f3c8a8ef2f9")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "923a34330207ed868b6388acf0f432ae989f1427")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "923a34330207ed868b6388acf0f432ae989f1427")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "0c2735eea5e0803921903dc5f7294dcdeece47d2")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "d01718e9bbdf385205fb9c713caa64864f249392")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "ed2b762241bbea03e374dc9dcd4fbe207c6b2ea4")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "ed2b762241bbea03e374dc9dcd4fbe207c6b2ea4")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "87ab7e47e343285f7afd42779c78551332b8fd84")
(package! wgrep)
