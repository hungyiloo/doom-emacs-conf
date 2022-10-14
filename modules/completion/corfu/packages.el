;; -*- no-byte-compile: t; -*-

(package! corfu :pin "dcdadf0f850d49e9190587bf357ac790a15b17ae")
(when (modulep! +orderless)
  (package! orderless :pin "8b9af2796fa0eb87eea4140bc08d16880a493803"))
(package! kind-icon :pin "cce18f777e2e6fedb09e50108ebb06810f436039")
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc" :branch "main"))
