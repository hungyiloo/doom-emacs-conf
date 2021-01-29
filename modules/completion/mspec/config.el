;;; completion/mspec/config.el -*- lexical-binding: t; -*-

(use-package! selectrum-prescient
  :config
  (selectrum-mode +1)
  ;;(use-package sudo-edit)
  ;;(global-set-key (kbd "M-r") #'selectrum-repeat)
  )
(use-package! prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package! consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind (("C-x M-:" . consult-complex-command)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c m" . consult-mode-command)
  ;;        ("C-x b" . consult-buffer)
  ;;        ("C-x 4 b" . consult-buffer-other-window)
  ;;        ("C-x 5 b" . consult-buffer-other-frame)
  ;;        ("C-x r x" . consult-register)
  ;;        ("C-x r b" . consult-bookmark)
  ;;        ("M-g g" . consult-goto-line)
  ;;        ("M-g M-g" . consult-goto-line)
  ;;        ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
  ;;        ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
  ;;        ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
  ;;        ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
  ;;        ("M-g r" . consult-ripgrep)      ;; or consult-grep, consult-ripgrep
  ;;        ("M-g f" . consult-find)          ;; or consult-fdfind, consult-locate
  ;;        ("M-g i" . consult-project-imenu) ;; or consult-imenu
  ;;        ("M-g e" . consult-error)
  ;;        ("M-s m" . consult-multi-occur)
  ;;        ("M-y" . consult-yank-pop)

  ;;        ("<f3>" . consult-ripgrep)
  ;;        ("C-s" . consult-line)

  ;;        ("<help> a" . consult-apropos))

  :defer t

  ;; The :init configuration is always executed (Not lazy!)

  :init
  (map! :leader
        "f r" #'consult-recent-file
        "i u" #'insert-char
        "s s" #'consult-line)

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  ;;(fset 'projectile-ripgrep 'consult-ripgrep)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally configure a function which returns the project root directory
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  ;;(consult-preview-mode)
  )

;; Enable Consult-Selectrum integration.
;; This package should be installed if Selectrum is used.
(use-package! consult-selectrum
  :after selectrum
  :demand t)

;; Optionally add the `consult-flycheck' command.
(use-package! consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))

(use-package! marginalia
  :bind (;; :map minibuffer-local-map
         ;;      ("C-M-a" . marginalia-cycle)
         ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
         ;;:map embark-general-map
         ;;    ("A" . marginalia-cycle)
        )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

(use-package! embark
  :config
  (add-hook 'embark-target-finders
            (defun current-candidate+category ()
              (when selectrum-active-p
                (cons (selectrum--get-meta 'category)
                      (selectrum-get-current-candidate)))))

  (add-hook 'embark-candidate-collectors
            (defun current-candidates+category ()
              (when selectrum-active-p
                (cons (selectrum--get-meta 'category)
                      (selectrum-get-current-candidates
                       ;; Pass relative file names for dired.
                       minibuffer-completing-file-name)))))

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  ;; The following is not selectrum specific but included here for convenience.
  ;; If you don't want to use which-key as a key prompter skip the following code.

  (setq embark-action-indicator
        (lambda (map) (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  :bind
  ("C-S-a" . embark-act))
