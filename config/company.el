;;; config/company.el -*- lexical-binding: t; -*-

(after! company
  ;; More responsive company autocompletion
  (setq company-idle-delay 0.1)
  (after! evil
    ;; Preserve my muscle memory and allow the escape key to
    ;; cancel the company popup without messing with evil state.
    ;; If no company popup is shown, escape will go back to normal state.
    (advice-add #'evil-normal-state
                :around
                (defun my/company-quit-before-evil-normal-state (orig-fun &rest args)
                  (if company-candidates
                      (company-cancel)
                    (apply orig-fun args))))))
