;;; config/flycheck.el -*- lexical-binding: t; -*-

(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map))

  (advice-add #'flycheck-buffer
              :around
              (defun my/flycheck-ignore-when-disabled-advice (orig-fun &rest args)
                (when flycheck-mode
                    (apply orig-fun args)))))
