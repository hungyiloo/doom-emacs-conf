;;; config/evil.el -*- lexical-binding: t; -*-

(after! evil
  ;; I'm a dirty javascripter, so I like 2-space indentation
  (setq evil-shift-width 2)

  ;; A lot of non-evil emacs packages expect the point
  ;; to be able to move past the last character of something.
  ;; I got tired of adding whitespace at the end of a file to
  ;; fix these edge cases.
  (setq evil-move-beyond-eol t)

  ;; Often I want to jump more than just within the line
  (setq evil-snipe-scope 'visible)

  ;; Repeat keys (a.k.a. smart f) was annoying me
  ;; I just use ; and , to go between snipe instances.
  (setq evil-snipe-repeat-keys nil)

  ;; Allow emacs's tree-like undo system
  (setq evil-undo-function #'undo)

  ;; This lets me undo things within an insert "session"
  (setq evil-want-fine-undo 't)

  ;; I think C-u is more useful as the universal prefix in insert mode.
  ;; If I really want to delete back to indentation exactly this way,
  ;; just d^
  (setq evil-want-C-u-delete nil)

  ;; Setting this to nil fixes small edge cases in evil-mc
  ;; but setting it to t fixes LARGE edge cases in evil-mc
  ;; TODO: Look into evil-mc single character visual selection to see
  ;; if it can be fixed when this flag is still set to true.
  (setq evil-move-cursor-back t)

  ;; Use emacs-style regexp everywhere to prevent confusion and
  ;; "unmatched" errors with curly braces when using evil-ex and evil-search
  ;; Might be related to this: https://github.com/emacs-evil/evil/issues/347
  (setq evil-ex-search-vim-style-regexp nil)

  (map!
   ;; A convenient binding for the psychic hippie-expand
   :i "C-?" #'hippie-expand

   ;; I actually end up using C-n and C-p for up/down line navigation a lot.
   :n "C-n" #'next-line
   :n "C-p" #'previous-line
   :n "C-S-p" #'evil-paste-pop
   :n "C-S-n" #'evil-paste-pop-next

   ;; TODO: The evil-mc hydra I wrote should really be in *this* file, or an evil-mc config file.
   (:when (featurep! :editor multiple-cursors)
    :prefix "g"
    :nv "z" #'my/mc-hydra/body)

   ;; TODO: This should really be in its own smartparens config file
   :prefix "gs"
   :nv "p" #'my/sp-hydra/body)

  ;; Fix ^ movement to also respect visual line mode.
  ;; It works for 0 and $ so why not ^?
  (when evil-respect-visual-line-mode
    (evil-define-minor-mode-key 'motion 'visual-line-mode
      "^"  #'evil-first-non-blank-of-visual-line
      "g^" #'evil-first-non-blank))

  ;; I like having evil search matches be in the middle of the screen
  (defun my/recenter (&rest ignored)
    (recenter))
  (advice-add #'evil-ex-search-forward :after #'my/recenter)
  (advice-add #'evil-ex-search-backward :after #'my/recenter))

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

(after! evil-mc
  ;; Prevents evil-mc from clearing registers when multiple cursors are created
  ;; NOTE: This might break named register yanking and pasting *during* evil-mc-mode
  (setq evil-mc-cursor-variables (mapcar
                                  (lambda (s)
                                    (remove 'register-alist (remove 'evil-markers-alist s)))
                                  evil-mc-cursor-variables)))
