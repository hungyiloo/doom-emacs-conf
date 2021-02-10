;;; config/evil.el -*- lexical-binding: t; -*-

(after! evil
  (setq evil-shift-width 2)
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  ;; (setq evil-escape-key-sequence "hl")
  ;; (setq evil-escape-unordered-key-sequence t)
  ;; (setq evil-escape-delay 0.05)
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-keys nil)
  (setq evil-undo-function #'undo)
  (setq evil-want-fine-undo 't)
  (setq evil-want-C-u-delete nil)
  (map!
   :i "C-S-SPC" #'hippie-expand
   :n "C-n" #'next-line
   :n "C-p" #'previous-line
   :n "C-S-p" #'evil-paste-pop
   :n "C-S-n" #'evil-paste-pop-next
   (:when (featurep! :editor multiple-cursors)
    :prefix "g"
    :nv "z" #'my-mc-hydra/body)
   :prefix "gs"
   :nv "p" #'my-sp-hydra/body)
  ;; Fix ^ movement to also respect visual line mode
  (when evil-respect-visual-line-mode
    (evil-define-minor-mode-key 'motion 'visual-line-mode
      "^"  #'evil-first-non-blank-of-visual-line
      "g^" #'evil-first-non-blank))
  (defun my-recenter (&rest ignored)
    (recenter))
  (advice-add #'evil-ex-search-forward :after #'my-recenter)
  (advice-add #'evil-ex-search-backward :after #'my-recenter))

(after! evil-collection
  ;; Fix regular linewise movement in org mode and outline mode.
  ;; Previously they were mapped to `outline-backward-same-level' and `outline-forward-same-level'.
  ;; But in org mode (where this matters) it's available via '[h' and ']h' already.
  ;; No need for gk and gj to be remapped.
  (evil-collection-define-key 'normal 'outline-mode-map
    "gk" nil
    "gj" nil))

(after! evil-goggles
  (setq evil-goggles-duration 0.2)
  (setq evil-goggles-enable-paste nil))
