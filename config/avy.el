;;a config/avy.el -*- lexical-binding: t; -*-

(after! avy
  ;; Configure avy to use colemak home row
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h))

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setq avy-dispatch-alist '((59 . avy-action-embark)
                             (?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?< . avy-action-teleport)
                             (?m . avy-action-mark)
                             (?w . avy-action-copy)
                             (?y . avy-action-yank)
                             (?Y . avy-action-yank-line)
                             (?= . avy-action-ispell)
                             (?z . avy-action-zap-to-char))))
