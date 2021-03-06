;;; config/vterm.el -*- lexical-binding: t; -*-

(after! vterm
  ;; Start vterm in normal mode always
  (evil-set-initial-state 'vterm-mode 'normal)

  (advice-add #'insert-for-yank
              :around
              (defun my/insert-for-yank-vterm-shim (orig-fun &rest args)
                (if (eq major-mode 'vterm-mode)
                    (let ((inhibit-read-only t))
                      (apply #'vterm-insert args))
                  (apply orig-fun args))))

  (after! consult
    (defun my/vterm-consult-yank-pop (&optional arg)
      "A `consult-yank-pop' wrapper for vterm compatibility."
      (interactive "p")
      (let ((inhibit-read-only t))
        (consult-yank-pop arg)))

    (map! :map 'vterm-mode-map
          [remap yank-pop] #'my/vterm-consult-yank-pop)))
