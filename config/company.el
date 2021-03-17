;;; config/company.el -*- lexical-binding: t; -*-

(after! company
  ;; More responsive company autocompletion
  (setq company-idle-delay 0.2)

  ;; REVIEW: This doesn't really feel good to me. Maybe I've gotten used to the
  ;; C-g way to cancel company already
  ;;
  ;; (after! evil
  ;;   ;; Preserve my muscle memory and allow the escape key to
  ;;   ;; cancel the company popup without messing with evil state.
  ;;   ;; If no company popup is shown, escape will go back to normal state.
  ;;   (advice-add #'evil-normal-state
  ;;               :around
  ;;               (defun my/company-quit-before-evil-normal-state (orig-fun &rest args)
  ;;                 (if company-candidates
  ;;                     (company-cancel)
  ;;                   (apply orig-fun args)))))

  ;; REVIEW: Is this aggressive approach better, or is doom's C-x C-s snippet completion already fine?
  ;; (advice-add #'company-complete-common-or-cycle
  ;;             :around
  ;;             (defun my/yas-expand-before-company-complete (orig-fun &rest args)
  ;;               "Attempts to yas-expand before falling back to the ORIG-FUN from company"
  ;;               (unless (yas-expand-from-trigger-key)
  ;;                 (apply orig-fun args))))
  )
