;;; config/ctrlf.el -*- lexical-binding: t; -*-

(ctrlf-mode 1)
(after! ctrlf
  ;; Clearer active search face
  (custom-set-faces!
    `(ctrlf-highlight-active :background ,(doom-color 'magenta 256) :foreground ,(doom-color 'base0) :weight bold))

  ;; Disable ctrlf in pdf-tools isearch
  (add-hook! 'pdf-isearch-minor-mode-hook
    (ctrlf-local-mode -1))

  ;; Clear evil ex search highlights when starting ctrlf search
  (defun my/ctrlf-clear-evil-ex-highlights-before (&rest _args)
    (+evil-disable-ex-highlights-h))
  (advice-add #'ctrlf-forward-literal :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-backward-literal :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-forward-regexp :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-backward-regexp :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-forward-symbol :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-forward-symbol-at-point :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-forward-fuzzy :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-forward-fuzzy-regexp :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-backward-fuzzy :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-backward-fuzzy-regexp :before #'my/ctrlf-clear-evil-ex-highlights-before)
  (advice-add #'ctrlf-occur :before #'my/ctrlf-clear-evil-ex-highlights-before))
