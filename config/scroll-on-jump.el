;;; config/scroll-on-jump.el -*- lexical-binding: t; -*-

(use-package! scroll-on-jump
  :commands (scroll-on-jump)
  :init
  (after! evil
    (scroll-on-jump-advice-add evil-undo)
    (scroll-on-jump-advice-add evil-redo)
    (scroll-on-jump-advice-add evil-jump-item)
    (scroll-on-jump-advice-add evil-jump-forward)
    (scroll-on-jump-advice-add evil-jump-backward)
    ;; (scroll-on-jump-advice-add evil-ex-search-forward)
    ;; (scroll-on-jump-advice-add evil-ex-search-backward)
    (scroll-on-jump-advice-add evil-ex-search-next)
    (scroll-on-jump-advice-add evil-ex-search-previous)
    (scroll-on-jump-advice-add evil-forward-paragraph)
    (scroll-on-jump-advice-add evil-backward-paragraph)
    (scroll-on-jump-advice-add evil-goto-mark)
    (scroll-on-jump-advice-add evil-goto-first-line)
    (scroll-on-jump-advice-add evil-goto-line)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-page-down)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-page-up)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
    (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))
  (after! evil-snipe
    (scroll-on-jump-advice-add evil-snipe-f)
    (scroll-on-jump-advice-add evil-snipe-F)
    (scroll-on-jump-advice-add evil-snipe-s)
    (scroll-on-jump-advice-add evil-snipe-S)
    (scroll-on-jump-advice-add evil-snipe-repeat)
    (scroll-on-jump-advice-add evil-snipe-repeat-reverse))
  (after! git-gutter
    (scroll-on-jump-advice-add git-gutter:next-diff)
    (scroll-on-jump-advice-add git-gutter:previous-diff)
    (scroll-on-jump-advice-add git-gutter:next-hunk)
    (scroll-on-jump-advice-add git-gutter:previous-hunk))
  (after! better-jumper
    (scroll-on-jump-advice-add better-jumper-jump-forward)
    (scroll-on-jump-advice-add better-jumper-jump-backward))
  (after! spell-fu
    (scroll-on-jump-advice-add spell-fu-goto-next-error)
    (scroll-on-jump-advice-add spell-fu-goto-previous-error))
  (after! flycheck
    (scroll-on-jump-advice-add flycheck-next-error)
    (scroll-on-jump-advice-add flycheck-previous-error))
  (after! evil-mc
    (scroll-on-jump-advice-add evil-mc-make-and-goto-next-match)
    (scroll-on-jump-advice-add evil-mc-make-and-goto-prev-match)
    (scroll-on-jump-advice-add evil-mc-skip-and-goto-next-match)
    (scroll-on-jump-advice-add evil-mc-skip-and-goto-prev-match)
    (scroll-on-jump-advice-add +multiple-cursors/evil-mc-undo-cursor))
  (after! goto-chg
    (scroll-on-jump-advice-add goto-last-change)
    (scroll-on-jump-advice-add goto-last-change-reverse))
  (after! lookup
    (scroll-on-jump-advice-add +lookup/definition))
  (after! consult
    (scroll-on-jump-advice-add consult--jump-1))
  (scroll-on-jump-advice-add exchange-point-and-mark)
  :config
  (setq scroll-on-jump-smooth nil))
