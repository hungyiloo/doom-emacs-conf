;;; completion/compres/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (after-init . selectrum-mode)
  :config
  (map! :leader
        "'" #'selectrum-repeat)
  (setq selectrum-extend-current-candidate-highlight t)

  (custom-set-faces!
    `(selectrum-current-candidate :background ,(doom-blend (doom-color 'highlight) (doom-color 'bg-alt) 0.175))
    `(selectrum-completion-docsig :foreground ,(doom-color 'base5) :italic nil)
    `(selectrum-primary-highlight :foreground ,(doom-color 'magenta) :bold t)
    `(selectrum-secondary-highlight :foreground ,(doom-color 'magenta 256) :inherit 'selectrum-primary-highlight :underline t)
    `(selectrum-completion-annotation :foreground ,(doom-color 'base5) :italic nil)
    `(marginalia-mode :foreground ,(doom-color 'blue))
    `(marginalia-on :foreground ,(doom-color 'green 256))
    `(marginalia-key :foreground ,(doom-color 'cyan))
    `(marginalia-off :foreground ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.4))
    `(marginalia-size :foreground ,(doom-color 'violet 256))
    `(marginalia-char :foreground ,(doom-color 'teal))
    `(marginalia-date :foreground ,(doom-color 'teal))
    `(marginalia-number :foreground ,(doom-color 'teal))
    `(marginalia-archive :foreground ,(doom-color 'yellow))
    `(marginalia-lighter :foreground ,(doom-color 'violet))
    `(marginalia-version :foreground ,(doom-color 'violet))
    `(marginalia-variable :foreground ,(doom-color 'teal 256))
    `(marginalia-modified :foreground ,(doom-color 'orange))
    `(marginalia-file-name :foreground ,(doom-color 'base5) :italic t)
    `(marginalia-installed :foreground ,(doom-color 'green))
    `(marginalia-file-modes :foreground ,(doom-color 'dark-blue))
    `(marginalia-file-owner :foreground ,(doom-color 'doc-comments 256))
    `(marginalia-documentation :foreground ,(doom-blend (doom-color 'base6) (doom-color 'base4) 0.4)))

  ;; Better minibuffer prepopulation using M-n
  (setq minibuffer-default-add-function #'+compres/minibuffer-default-add-function)

  (map! :map selectrum-minibuffer-map
       "C-d" #'selectrum-next-page
       "C-u" #'selectrum-previous-page
       "C-k" #'kill-line)

  (after! magit
    ;; Without setting this, magit completing-read candidates won't be sorted with prescient
    (setq magit-completing-read-function #'selectrum-completing-read)))

(use-package! orderless
  :defer t
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq orderless-style-dispatchers `(,(defun +compress--orderless-without-if-at-bang (pattern _index _total)
                                         (when (string-prefix-p "@!" pattern)
                                           `(orderless-without-literal . ,(substring pattern 2))))))
  (custom-set-faces!
    `(orderless-match-face-0 :foreground ,(doom-color 'magenta) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-1 :foreground ,(doom-color 'yellow) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-2 :foreground ,(doom-color 'cyan) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-3 :foreground ,(doom-color 'green) :bold t :background ,(doom-color 'base0))))

(use-package! prescient
  :after selectrum
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package! consult
  :defer t
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  (define-key!
    [remap apropos] #'consult-apropos
    [remap bookmark-jump] #'consult-bookmark
    [remap goto-line] #'consult-goto-line
    [remap imenu] #'consult-imenu
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    [remap man] #'consult-man
    [remap +default/yank-pop] #'consult-yank-pop
    [remap yank-pop] #'consult-yank-pop
    [remap locate] #'consult-locate
    [remap load-theme] #'consult-theme
    [remap recentf-open-files] #'consult-recent-file
    [remap isearch-edit-string] #'consult-isearch
    [remap repeat-complex-command] #'consult-complex-command
    [remap project-switch-to-buffer] #'+compres/consult-project-buffer
    [remap +default/search-cwd] #'+compres/consult-ripgrep-cwd
    [remap +default/search-other-cwd] #'+compres/consult-ripgrep-other-cwd
    [remap +default/search-project] #'+compres/consult-ripgrep-dwim
    [remap +default/search-project-for-symbol-at-point] #'+compres/consult-ripgrep-symbol-at-point
    [remap +default/search-buffer] #'+compres/consult-line-dwim
    [remap +default/search-other-project] #'+compres/consult-ripgrep-other-project-dwim
    [remap +default/find-in-notes] #'+compres/consult-ripgrep-notes
    [remap +default/find-file-under-here] #'+compres/consult-find-under-here
    [remap +default/org-notes-search] #'+compres/consult-ripgrep-notes
    [remap +default/search-notes-for-symbol-at-point] #'+compres/consult-ripgrep-notes-symbol-at-point)

  :config

  ;; Don't be so aggresive with previews
  (setq consult-config `((consult-buffer :preview-key ,(kbd "C-."))
                         (consult-bookmark :preview-key ,(kbd "C-."))
                         (consult-grep :preview-key ,(kbd "C-."))
                         (consult-ripgrep :preview-key ,(kbd "C-."))
                         (consult-recent-file :preview-key ,(kbd "C-."))
                         (+compres/consult-project-buffer :preview-key ,(kbd "C-."))
                         (+compres/consult-ripgrep-dwim :preview-key ,(kbd "C-."))
                         (+compres/consult-ripgrep-symbol-at-point :preview-key ,(kbd "C-."))
                         (+compres/consult-ripgrep-cwd :preview-key ,(kbd "C-."))
                         (+compres/consult-ripgrep-other-cwd :preview-key ,(kbd "C-."))
                         (+compres/consult-ripgrep-other-project-dwim :preview-key ,(kbd "C-."))
                         (+compres/consult-find-under-here :preview-key ,(kbd "C-."))))

  ;; Ensure consult-recent-file returns a list of files on startup.
  ;; Without this, sometimes it can be empty on startup because it hasn't been loaded yet?
  ;; Not sure how to more elgantly trigger a load.
  (recentf-load-list)

  ;; Optionally configure a function which returns the project root directory
  (setq consult-project-root-function #'doom-project-root)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; use fd instead of find
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  ;; Bind a key for narrowing in consult.
  ;; Mainly this is to access the SPC combination to clear the filters.
  ;; Without this, there seems to be no way to widen after filtering `consult-buffer'
  (setq consult-narrow-key (kbd "S-SPC"))

  ;; Fix `consult-imenu' narrowing and add a few more values
  (setq consult-imenu-narrow
        '((emacs-lisp-mode . ((?f . "Functions")
                              (?m . "Macros")
                              (?p . "Package") ; Singular "Package" is what works for me
                              (?t . "Types")
                              (?v . "Variables")
                              ;; These are added by me
                              (?a . "Advice")
                              (?S . "Section") ; If the key is set as ?s, it doesn't display in which-key properly. Why?
                              ))))
  ;; Fix `consult-imenu' grouping, should be in line with the narrowing config above
  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions" :types
                           ((?f "Functions" font-lock-function-name-face)
                            (?m "Macros" font-lock-function-name-face)
                            (?p "Package" font-lock-constant-face) ; Singular Package is what works for me
                            (?t "Types" font-lock-type-face)
                            (?v "Variables" font-lock-variable-name-face)
                            ;; These are added by me
                            (?a "Advice" font-lock-function-name-face)
                            (?S "Section" font-lock-constant-face)))))

  ;; Use expanded file name to compare with project root
  (setq consult--source-project-file
        `(:name      "Project File"
          :narrow    (?p . "Project")
          :hidden    t
          :category  file
          :face      consult-file
          :history   file-name-history
          :action    ,#'consult--file-action
          :enabled   ,(lambda () (and consult-project-root-function
                                 recentf-mode))
          :items
          ,(lambda ()
             (when-let (root (funcall consult-project-root-function))
               (let ((len (length root))
                     (inv-root (propertize root 'invisible t))
                     (ht (consult--cached-buffer-file-hash)))
                 (mapcar (lambda (x)
                           (concat inv-root (substring x len)))
                         (seq-filter (lambda (x)
                                       (and (> (length x) len)
                                            (not (gethash x ht))
                                            (string-prefix-p root x)))
                                     (mapcar #'expand-file-name recentf-list))))))))



  ;; Integrate with evil jumping
  (after! evil
    (evil-set-command-property #'consult--jump :jump t))

  ;; Better than `org-set-tags-command'
  (after! org
    (defalias #'org-set-tags-command #'+compres/consult-org-set-tags))

  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=250 --no-heading --line-number . -e ARG OPTS -S"))

;; Enable Consult-Selectrum integration.
;; This package should be installed if Selectrum is used.
(use-package! consult-selectrum
  :after selectrum
  :demand t)

;; Optionally add the `consult-flycheck' command.
(use-package! consult-flycheck
  :commands consult-flycheck
  :init (map!
         :map flycheck-command-map
         "z" #'consult-flycheck))

(use-package! marginalia
  :hook (after-init . marginalia-mode)
  :config
  (map! :map minibuffer-local-map
        "C-M-a" #'marginalia-cycle)

  (after! embark
    (map! :map embark-general-map
        "A" #'marginalia-cycle))
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; Classify project file choosers as 'project-file'
  (add-to-list 'marginalia-command-categories '(project-find-file . project-file))
  ;; But switching projects are just regular files/dirs
  (add-to-list 'marginalia-command-categories '(project-switch-project . file))

  ;; Speed up marginalia project resolution
  (add-hook! 'minibuffer-setup-hook
             ;; Cache the project root when the minibuffer is set up
             (setq +compres--marginalia-cached-project-root (doom-project-root)))
  (advice-add #'marginalia-annotate-project-file
              :around
              (defun +compres/marginalia-fast-annotate-project-file (ignored cand)
                ;; Find the cached project root, use it to annotate the file
                ;; instead of relying on the old `marginalia-annotate-project-file' root determination
                (marginalia-annotate-file (expand-file-name cand +compres--marginalia-cached-project-root)))))

(use-package! embark
  :bind
  ("C-S-a" . embark-act)

  :config
  ;; The following is not selectrum specific but included here for convenience.
  ;; If you don't want to use which-key as a key prompter skip the following code.
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  ;; FIXME: Ensure selectrum candidates are refreshed after embark act without exit (using C-u)
  ;; (add-hook! #'embark-post-action-hook #'consult-selectrum--refresh)

  ;; Allow `embark-export' on project-file category
  (add-to-list 'embark-exporters-alist '(project-file . +compres/embark-export-prj-dired))

  ;; Add support for project file actions
  (add-to-list 'embark-keymap-alist '(project-file . embark-project-file-map))
  (embark-define-keymap embark-project-file-map
    "Keymap for Embark project file actions."
    ("f" +compres/prj-find-file)
    ("o" +compres/prj-find-file-other-window)
    ("d" +compres/prj-delete-file)
    ("r" +compres/prj-rename-file)
    ("c" +compres/prj-copy-file)
    ("=" +compres/prj-ediff-files)
    ("I" +compres/prj-embark-insert-relative-path)
    ("W" +compres/prj-embark-save-relative-path))

  ;; Make embark export occur buffers less overwhelming by putting them in popups
  ;; (set-popup-rule!
  ;;   "^\\*Embark Export Occur"
  ;;   :height 0.4
  ;;   :quit t)
  )

(use-package! embark-consult
  :after (embark consult))

;; Better xref experience in absence of ivy
(after! xref
  (set-popup-rule!
    "^\\*xref"
    :height 20
    :quit t)

  ;; Close the xref popup after going to the xref
  (advice-add #'xref-goto-xref
              :around
              (defun +compres/close-popup-on-goto-xref (orig-fun &rest args)
                (let ((orig-window (selected-window)))
                  (apply orig-fun args)
                  (+popup/close orig-window))))

  (custom-set-faces!
    `(xref-match :foreground ,(doom-color 'magenta) :bold t :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.3))))

(after! spell-fu
  (define-key!
   [remap ispell-word] #'+compres/spell-correct))
