(use-package! magit
  :hook (magit-mode . my-magit-mode-hook)
  :config
  ;; Wider fringe (emacs default) for better magit support
  ;; source: https://emacs.stackexchange.com/a/47679
  (defun my-magit-window-config ()
    "Used in `window-configuration-change-hook' to configure fringes for Magit."
    (set-window-fringes nil 20 0))
  (defun my-magit-mode-hook ()
    "Custom `magit-mode' behaviours."
    (add-hook 'window-configuration-change-hook
              'my-magit-window-config nil :local))
  ;; fix tab key not working in magit-refs-mode
  (map! :map magit-refs-mode-map
        :n "<tab>" #'magit-section-toggle))
