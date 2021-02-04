(after! ediff
  ;; Include ediff buffers in solaire-mode so they look the same
  ;; as regular editing buffers
  (add-hook! 'ediff-prepare-buffer-hook #'solaire-mode))
