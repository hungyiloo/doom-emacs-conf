;;; config/org-alert.el -*- lexical-binding: t; -*-

(after! alert
  (alert-define-style 'wsl :title "WSL Toast"
                      :notifier
                      (lambda (info)
                        (toast (plist-get info :title) (plist-get info :message))))
  (setq alert-default-style 'wsl))

(use-package! org-alert
  :after org
  :config
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notification-title "Journal Reminder"))
