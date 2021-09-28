;;; config/ediff.el -*- lexical-binding: t; -*-

(after! ediff
  ;; Include ediff buffers in solaire-mode so they look the same
  ;; as regular editing buffers
  (add-hook! 'ediff-prepare-buffer-hook #'solaire-mode)

  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-&-to-ediff-mode-map () (define-key ediff-mode-map "&" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook #'add-&-to-ediff-mode-map))
