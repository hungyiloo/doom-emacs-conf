(use-package! eyebrowse
  :commands (my-eyebrowse-open-project
             my-eyebrowse-switch-buffer
             eyebrowse-create-window-config
             eyebrowse-create-named-window-config
             eyebrowse-rename-window-config
             eyebrowse-switch-to-window-config
             eyebrowse-switch-to-window-config-0
             eyebrowse-switch-to-window-config-1
             eyebrowse-switch-to-window-config-2
             eyebrowse-switch-to-window-config-3
             eyebrowse-switch-to-window-config-4
             eyebrowse-switch-to-window-config-5
             eyebrowse-switch-to-window-config-6
             eyebrowse-switch-to-window-config-7
             eyebrowse-switch-to-window-config-8
             eyebrowse-switch-to-window-config-9)
  :init
  (map!
   :n "[w"   #'eyebrowse-prev-window-config
   :n "]w"   #'eyebrowse-next-window-config)
  (map! :leader
        "SPC" #'project-find-file
        (:prefix-map ("p" . "project")
         "p" #'project-switch-project)
        "<tab> 0" #'eyebrowse-switch-to-window-config-0
        "<tab> 1" #'eyebrowse-switch-to-window-config-1
        "<tab> 2" #'eyebrowse-switch-to-window-config-2
        "<tab> 3" #'eyebrowse-switch-to-window-config-3
        "<tab> 4" #'eyebrowse-switch-to-window-config-4
        "<tab> 5" #'eyebrowse-switch-to-window-config-5
        "<tab> 6" #'eyebrowse-switch-to-window-config-6
        "<tab> 7" #'eyebrowse-switch-to-window-config-7
        "<tab> 8" #'eyebrowse-switch-to-window-config-8
        "<tab> 9" #'eyebrowse-switch-to-window-config-9
        "<tab> d" #'my-eyebrowse-close-workspace
        "<tab> D" #'eyebrowse-close-window-config
        "<tab> p" #'my-eyebrowse-open-project
        "<tab> r" #'eyebrowse-rename-window-config
        "<tab> ." #'eyebrowse-switch-to-window-config
        "<tab> <tab>" #'eyebrowse-switch-to-window-config
        "<tab> n" #'eyebrowse-create-window-config
        "<tab> N" #'eyebrowse-create-named-window-config
        "<tab> `" #'eyebrowse-last-window-config
        "<tab> [" #'eyebrowse-prev-window-config
        "<tab> ]" #'eyebrowse-next-window-config
        "," #'consult-buffer
        "<" #'project-switch-to-buffer)
  :config
  (eyebrowse-mode t)

  (after! marginalia
    ;; Opening projects has a category type of "file", not "project-file"
    (add-to-list 'marginalia-command-categories '(my-eyebrowse-open-project . file)))

  (defun my-eyebrowse-close-workspace ()
    "Closes all buffers in the current project (approximating a workspace)
and then closes the window config"
    (interactive)
    (if (doom-project-p)
        (when (yes-or-no-p "Close the project along with the workspace?")
          (call-interactively #'projectile-kill-buffers)
          (eyebrowse-close-window-config))
      (eyebrowse-close-window-config)))

  (defun my-eyebrowse-open-project ()
    "Creates a window config, open a project and name the eyebrowse slot to match the project name"
    (interactive)
    (eyebrowse-create-window-config)
    (condition-case nil
        (progn
          (call-interactively #'project-switch-project)
          (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-project-name)))
      (quit (eyebrowse-close-window-config))))

  (defun my-eyebrowse-switch-buffer ()
    "Switch buffer depending on project if we're in one"
    (interactive)
    (if (doom-project-p)
        (call-interactively #'project-switch-to-buffer)
      (call-interactively #'consult-buffer))))
