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
   :n "C-M-y" #'evil-paste-pop-next

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
  ;; NOTE: This might break named register jumping, yanking and pasting *during* evil-mc-mode
  (setq evil-mc-cursor-variables (mapcar
                                  (lambda (s)
                                    (remove 'register-alist (remove 'evil-markers-alist s)))
                                  evil-mc-cursor-variables))

  ;; Redefine this function to fix cursor misalignment issues.
  ;; e.g. With multiple cursors, visually select one character and change.
  ;;      With the original `evil-mc-execute-evil-change' the fake cursors would jump one
  ;;      character to the left, incorrectly.
  (defun evil-mc-execute-evil-change ()
    "Execute an `evil-change' comand."
    (let ((point (point)))
      (evil-with-state normal
        (unless (eq point (point-at-bol))
          (evil-forward-char 1 nil t)) ; Perhaps this behavior depends on `evil-move-cursor-back'?
        (evil-mc-execute-with-region-or-macro 'evil-change)
        (evil-maybe-remove-spaces nil))))

  ;; FIXME: This advice doesn't move the point like intended. Why?
  ;; (advice-add #'evil-mc-set-pattern
  ;;             :before
  ;;             (defun my/evil-mc-linewise-set-pattern-shim (&rest ignored)
  ;;               (when (and (evil-visual-state-p)
  ;;                          (eq (evil-visual-type) 'line))
  ;;                 (call-interactively #'move-end-of-line))))
  )
