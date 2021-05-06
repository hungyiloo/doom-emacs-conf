;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "05aa11bca37db1751c86fe78f784741be5b1a066")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "05aa11bca37db1751c86fe78f784741be5b1a066")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "f22d7f6cc15fe0c3744c53686160af7579a4799a")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "f22d7f6cc15fe0c3744c53686160af7579a4799a")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "2437a6f0abada9de122c4ac3b22bf26deb194185")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "8629ab5a6de572ada9dd5b18162a393969d9ebdf")
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
