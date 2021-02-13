;;; config/quickrun.el -*- lexical-binding: t; -*-

(use-package! quickrun
  :init
  (map! :leader
        (:prefix-map ("c" . "code")
         :desc "Quickrun eval replace" "q" #'+eval:replace-region
         :desc "Elisp eval replace" "E" #'my-region-and-replace))

  :commands (quickrun quickrun-region quickrun-replace-region my-region-and-replace)

  :config
  (advice-remove #'quickrun--outputter-replace-region #'+eval--quickrun-fix-evil-visual-region-a)

  (evil-define-operator my-region-and-replace (beg end)
    "Evaluate selection and replace it with its result."
    :move-point nil
    (interactive "<r>")
    (kill-region beg end)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))
