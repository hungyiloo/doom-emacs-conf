;;; config/projectile.el -*- lexical-binding: t; -*-

(after! project
  (map! :map project-prefix-map
        "m" #'magit-status
        "s" #'consult-ripgrep
        "v" #'projectile-run-vterm)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-ripgrep "Ripgrep")
          (project-dired "Dired")
          (magit-status "Magit")
          (projectile-run-vterm "VTerm"))))

(after! projectile
  (setq projectile-kill-buffers-filter 'kill-all))
