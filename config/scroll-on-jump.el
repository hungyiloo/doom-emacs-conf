;;; config/scroll-on-jump.el -*- lexical-binding: t; -*-

(use-package! scroll-on-jump
  :commands (scroll-on-jump my-global-smooth-scroll-mode)
  :init
  (define-minor-mode my-global-smooth-scroll-mode
    "Smooth scrolling on jumping on common navigation commands"
    :lighter " smooth"
    :global t
    (let ((jump-action (if my-global-smooth-scroll-mode
                           (lambda (fn)
                             (require 'scroll-on-jump)
                             (advice-add fn :around #'scroll-on-jump-advice--wrapper))
                         (lambda (fn) (advice-remove fn #'scroll-on-jump-advice--wrapper))))
          (jump-with-scroll-action (if my-global-smooth-scroll-mode
                                       (lambda (fn)
                                         (require 'scroll-on-jump)
                                         (advice-add fn :around #'scroll-on-jump-advice--with-scroll-wrapper))
                                     (lambda (fn) (advice-remove fn #'scroll-on-jump-advice--with-scroll-wrapper)))))
      (after! evil
        (funcall jump-action #'evil-undo)
        (funcall jump-action #'evil-redo)
        (funcall jump-action #'evil-jump-item)
        (funcall jump-action #'evil-jump-forward)
        (funcall jump-action #'evil-jump-backward)
        ;; (funcall jump-action #'evil-ex-search-forward)
        ;; (funcall jump-action #'evil-ex-search-backward)
        (funcall jump-action #'evil-ex-search-next)
        (funcall jump-action #'evil-ex-search-previous)
        (funcall jump-action #'evil-forward-paragraph)
        (funcall jump-action #'evil-backward-paragraph)
        (funcall jump-action #'evil-goto-mark)
        (funcall jump-action #'evil-goto-first-line)
        (funcall jump-action #'evil-goto-line)
        (funcall jump-with-scroll-action #'evil-scroll-down)
        (funcall jump-with-scroll-action #'evil-scroll-up)
        (funcall jump-with-scroll-action #'evil-scroll-page-down)
        (funcall jump-with-scroll-action #'evil-scroll-page-up)
        (funcall jump-with-scroll-action #'evil-scroll-line-to-center)
        (funcall jump-with-scroll-action #'evil-scroll-line-to-top)
        (funcall jump-with-scroll-action #'evil-scroll-line-to-bottom))
      (after! evil-snipe
        (funcall jump-action #'evil-snipe-f)
        (funcall jump-action #'evil-snipe-F)
        (funcall jump-action #'evil-snipe-s)
        (funcall jump-action #'evil-snipe-S)
        (funcall jump-action #'evil-snipe-repeat)
        (funcall jump-action #'evil-snipe-repeat-reverse))
      (after! git-gutter
        (funcall jump-action #'git-gutter:next-diff)
        (funcall jump-action #'git-gutter:previous-diff)
        (funcall jump-action #'git-gutter:next-hunk)
        (funcall jump-action #'git-gutter:previous-hunk))
      (after! better-jumper
        (funcall jump-action #'better-jumper-jump-forward)
        (funcall jump-action #'better-jumper-jump-backward))
      (after! spell-fu
        (funcall jump-action #'spell-fu-goto-next-error)
        (funcall jump-action #'spell-fu-goto-previous-error))
      (after! flycheck
        (funcall jump-action #'flycheck-next-error)
        (funcall jump-action #'flycheck-previous-error))
      (after! evil-mc
        (funcall jump-action #'evil-mc-make-and-goto-next-match)
        (funcall jump-action #'evil-mc-make-and-goto-prev-match)
        (funcall jump-action #'evil-mc-skip-and-goto-next-match)
        (funcall jump-action #'evil-mc-skip-and-goto-prev-match)
        (funcall jump-action #'+multiple-cursors/evil-mc-undo-cursor))
      (after! goto-chg
        (funcall jump-action #'goto-last-change)
        (funcall jump-action #'goto-last-change-reverse))
      (after! lookup
        (funcall jump-action #'+lookup/definition))
      (after! consult
        (funcall jump-action #'consult--jump-1))
      (funcall jump-action #'exchange-point-and-mark)))
  :config
  (setq scroll-on-jump-smooth nil))

(add-hook! 'doom-first-buffer-hook
           (my-global-smooth-scroll-mode +1))
