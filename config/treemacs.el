(use-package! treemacs
  :hook (treemacs-mode . hide-mode-line-mode) ; hide modeline in treemacs
  :init
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  :config
  (setq treemacs-wrap-around nil))
