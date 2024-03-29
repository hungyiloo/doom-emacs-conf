;;; config/evil.el -*- lexical-binding: t; -*-

(after! evil
  ;; I'm a dirty javascripter, so I like 2-space indentation
  (setq-default evil-shift-width 2)

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
  ;; (which I've fixed without needing to change `evil-move-cursor-back'! See
  ;; the redefinition of `evil-mc-execute-evil-change' in the evil-mc config)
  ;;
  ;; but setting it to t fixes LARGE edge cases in evil-mc
  ;; (which would be difficult for me to patch)
  (setq evil-move-cursor-back t)

  ;; Use emacs-style regexp everywhere to prevent confusion and
  ;; "unmatched" errors with curly braces when using evil-ex and evil-search
  ;; Might be related to this: https://github.com/emacs-evil/evil/issues/347
  (setq evil-ex-search-vim-style-regexp nil)

  (map!
   ;; I actually end up using C-n and C-p for up/down line navigation a lot.
   :n "C-n" #'next-line
   :n "C-p" #'previous-line

   ;; If evil-paste-pop doesn't resolve (because last action wasn't a paste)
   ;; then fall back to yank-pop/consult-yank-pop as a convenience.
   :n "M-y" (cmd! (condition-case nil
                      (call-interactively #'evil-paste-pop)
                    (error (call-interactively #'consult-yank-replace))))
   :n "M-Y" (cmd! (condition-case nil
                      (call-interactively #'evil-paste-pop-next)
                    (error (call-interactively #'consult-yank-replace))))

   ;; TODO: The evil-mc hydra I wrote should really be in *this* file, or an evil-mc config file.
   (:when (modulep! :editor multiple-cursors)
    :prefix "g"
    :nv "z" #'my/mc-hydra/body)

   ;; TODO: This should really be in its own smartparens config file
   (:prefix "gs"
    :nv "p" #'my/sp-hydra/body)

   (:prefix "gs"
    :nv "z" #'my/tarzan-hydra/body)

   (:prefix "g"
    :nv "|" #'my/evil-duplicate-operator)

   (:prefix "g"
    :nv "r" #'+eval:region
    :nv "R" #'+eval:replace-region))

  ;; Fix ^ movement to also respect visual line mode.
  ;; It works for 0 and $ so why not ^?
  (when evil-respect-visual-line-mode
    (evil-define-minor-mode-key 'motion 'visual-line-mode
      "^"  #'evil-first-non-blank-of-visual-line
      "g^" #'evil-first-non-blank))

  ;; Evil text objects for inner/outer line selection
  (defun my/evil-select-line (type inner-p)
    (evil-range (save-excursion
                  (if inner-p (beginning-of-line-text) (beginning-of-line))
                  (point))
                (line-end-position)
                type))
  (evil-define-text-object
    evil-inner-line (count &optional beg end type)
    "Inner text object for line."
    (my/evil-select-line type t))
  (evil-define-text-object
    evil-outer-line (count &optional beg end type)
    "Outer text object for line."
    (my/evil-select-line type nil))
  (map! :map evil-inner-text-objects-map "l" #'evil-inner-line)
  (map! :map evil-outer-text-objects-map "l" #'evil-outer-line)

  ;; Clearer evil active search face
  (custom-set-faces!
    `(evil-ex-search :background ,(doom-color 'magenta 256) :foreground ,(doom-color 'base0) :weight bold))
  (defun my/evil-ex-search-highlight-current (&rest _args)
    (require 'ov)
    (ov-clear 'ov-evil-ex-active-search)
    (when-let* ((pattern (car evil-ex-search-pattern))
                (matched (save-excursion (search-forward-regexp pattern nil t)))
                (beg (match-beginning 0))
                (end (match-end 0)))
      (ov beg end
          'face 'evil-ex-search
          'ov-evil-ex-active-search t
          'evaporate t
          'priority 1002)))
  (defun my/evil-ex-search-clear-current-highlight (&rest _args)
    (ov-clear 'ov-evil-ex-active-search))
  (advice-add #'evil-ex-search-next :after #'my/evil-ex-search-highlight-current)
  (advice-add #'evil-ex-search-previous :after #'my/evil-ex-search-highlight-current)
  (advice-add #'evil-ex-search-forward :after #'my/evil-ex-search-highlight-current)
  (advice-add #'evil-ex-search-backward :after #'my/evil-ex-search-highlight-current)
  (advice-add #'+evil-disable-ex-highlights-h :after #'my/evil-ex-search-clear-current-highlight)

  (evil-define-operator my/evil-duplicate-operator (beg end type)
    (interactive "<R>")
    (message "%s" type)
    (save-excursion
      (evil-exit-visual-state)
      (let ((linewise (memq type '(line screen-line))))
        (if linewise
            (evil-yank-lines beg end)
          (progn
            (evil-yank beg end)))
        (goto-char end))
      (evil-paste-before 1))
    (goto-char (car (last evil-last-paste))))

  ;; Fix left and right char movement in grep-mode for evil
  (map! :map grep-mode-map
        :nv "h" #'evil-backward-char
        :nv "l" #'evil-forward-char))

(after! evil-collection
  ;; Fix regular linewise movement in org mode and outline mode.
  ;; Previously they were mapped to `outline-backward-same-level' and `outline-forward-same-level'.
  ;; But in org mode (where this matters) it's available via '[h' and ']h' already.
  ;; No need for gk and gj to be remapped.
  (evil-collection-define-key 'normal 'outline-mode-map
    "gk" nil
    "gj" nil))

(use-package! evil-goggles
  :init
  (setq evil-goggles-duration 0.2
        evil-goggles-enable-paste t
        evil-goggles-enable-delete t
        evil-goggles-enable-change t)
  :config
  (evil-goggles-use-magit-faces))

(after! evil-mc
  ;; Prevents evil-mc from clearing registers when multiple cursors are created
  ;; NOTE: This might break named register jumping, yanking and pasting *during* evil-mc-mode
  (setq evil-mc-cursor-variables
        (mapcar
          (lambda (s)
            (remove 'register-alist
                    (remove 'evil-markers-alist
                            (remove evil-was-yanked-without-register s))))
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

  (after! smartparens
    ;; Very basic, barely workable evil-mc integration with my smartparens hydra
    (dolist (sp-command '(my/sp-hydra/sp-up-sexp
                          my/sp-hydra/sp-copy-sexp
                          my/sp-hydra/sp-down-sexp
                          my/sp-hydra/sp-join-sexp
                          my/sp-hydra/sp-kill-sexp
                          my/sp-hydra/sp-next-sexp
                          my/sp-hydra/sp-split-sexp
                          my/sp-hydra/sp-wrap-curly
                          my/sp-hydra/sp-wrap-round
                          my/sp-hydra/sp-raise-sexp
                          my/sp-hydra/sp-clone-sexp
                          my/sp-hydra/sp-wrap-square
                          my/sp-hydra/sp-splice-sexp
                          my/sp-hydra/sp-end-of-sexp
                          my/sp-hydra/sp-forward-sexp
                          my/sp-hydra/sp-backward-sexp
                          my/sp-hydra/sp-convolute-sexp
                          my/sp-hydra/sp-transpose-sexp
                          my/sp-hydra/sp-kill-whole-line
                          my/sp-hydra/sp-beginning-of-sexp
                          my/sp-hydra/sp-forward-barf-sexp
                          my/sp-hydra/sp-forward-slurp-sexp
                          my/sp-hydra/sp-backward-barf-sexp
                          my/sp-hydra/sp-backward-slurp-sexp
                          my/sp-hydra/sp-splice-sexp-killing-forward
                          my/sp-hydra/sp-splice-sexp-killing-backward))
      (add-to-list 'evil-mc-custom-known-commands `(,sp-command (:default . evil-mc-execute-call))))))

(after! evil-nerd-commenter
  ;; Detect TSX files as JSX for commenting purposes
  (advice-add #'evilnc-html-jsx-p
              :around
              (defun my/evilnc-html-jsx-p (&rest _ignored)
                (and buffer-file-name
                     (string-match-p "\.[jt]sx?$" buffer-file-name)))))
