;;; config/magit.el -*- lexical-binding: t; -*-

(after! magit
  ;; fix tab key not working in magit-refs-mode
  (map! :map magit-refs-mode-map
        :n "<tab>" #'magit-section-toggle)

  (map! :leader
        (:prefix-map ("g" . "git")
         "p" #'my/magit-other-project))

  (map! :map magit-mode-map
        :n "Z" #'magit-stash
        :n "*" #'magit-worktree)

  (defun my/magit-other-project ()
    "Opens magit for another project of user's selection."
    (interactive)
    (let ((default-directory
            (if-let (projects (projectile-relevant-known-projects))
                (completing-read "Select project to open magit for: " projects nil t)
              (user-error "There are no known projects"))))
      (magit))))
