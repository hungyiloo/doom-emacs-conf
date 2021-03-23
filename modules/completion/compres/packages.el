;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! embark
  :recipe (:host github :repo "oantolin/embark")
  :pin "59e3626317725ba7a452562a9a84b8add77f9daa")
(package! embark-consult
  :recipe (:host github :repo "oantolin/embark")
  :pin "59e3626317725ba7a452562a9a84b8add77f9daa")
(package! consult
  :recipe (:host github :repo "minad/consult")
  :pin "70b795e948a52499722c244708db84d1d22f09cf")
(package! consult-flycheck
  :recipe (:host github :repo "minad/consult")
  :pin "70b795e948a52499722c244708db84d1d22f09cf")
(package! marginalia
  :recipe (:host github :repo "minad/marginalia")
  :pin "321feea44580655f2ba72a3755545d8996b7e511")
(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum")
  :pin "7cb5fb47f71eb29104f7ce0b82e6a1d1935206d3")
(package! prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "8573df977eaceffc6607b7242ff8c0dab02aad65")
(package! selectrum-prescient
  :recipe (:host github :repo "raxod502/prescient.el")
  :pin "8573df977eaceffc6607b7242ff8c0dab02aad65")
(package! orderless
  :recipe (:host github :repo "oantolin/orderless")
  :pin "9d5b95f40275dc47a57e9d4ee9b9994ee3a4b426")
(package! wgrep)
