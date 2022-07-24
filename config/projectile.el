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
  (setq projectile-kill-buffers-filter 'kill-all

        ;; Try to fix regression with error on --strip-cwd-prefix
        ;; (I'm using an older version of fd?)
        ;; https://github.com/bbatsov/projectile/pull/1784
        projectile-generic-command
        (cond
         ;; we prefer fd over find
         ((executable-find "fd")
          "fd . -0 --type f --color=never")
         ;; fd's executable is named fdfind is some Linux distros (e.g. Ubuntu)
         ((executable-find "fdfind")
          "fdfind . -0 --type f --color=never")
         ;; with find we have to be careful to strip the ./ from the paths
         ;; see https://stackoverflow.com/questions/2596462/how-to-strip-leading-in-unix-find
         (t "find . -type f | cut -c3- | tr '\\n' '\\0'"))))
