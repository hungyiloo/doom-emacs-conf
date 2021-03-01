;;; completion/compres/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (after-init . selectrum-mode)
  :config
  (map! :leader
        "'" #'selectrum-repeat)
  (setq selectrum-extend-current-candidate-highlight t)

  ;; These values fix issues with selectrum candidates being cut off
  ;; REVIEW: See when this is fixed properly upstream and remove?
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-max-window-height 13) ; one more than the desired number of candidates
  (setq selectrum-num-candidates-displayed 12)

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

  ;; (after! org
  ;;   ;; Better selectrum integration with `org-set-tags-command'
  ;;   ;; but the replacement command `my/consult-org-set-tags' below might be better
  ;;   (defun +compres/org-set-tags-command-multiple (orig &optional arg)
  ;;     (cl-letf (((symbol-function #'completing-read)
  ;;                (lambda (prompt collection &optional predicate require-match initial-input
  ;;                           hist def inherit-input-method)
  ;;                  (when initial-input
  ;;                    (setq initial-input
  ;;                          (replace-regexp-in-string
  ;;                           ":" ","
  ;;                           (replace-regexp-in-string
  ;;                            "\\`:" "" initial-input))))
  ;;                  (let ((res (completing-read-multiple
  ;;                              prompt collection predicate require-match initial-input
  ;;                              hist def inherit-input-method)))
  ;;                    (mapconcat #'identity res ":")))))
  ;;       (let ((current-prefix-arg arg))
  ;;         (call-interactively orig))))
  ;;   (advice-add #'org-set-tags-command :around #'+compres/org-set-tags-command-multiple))
  )

(use-package! orderless
  :defer t
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (custom-set-faces!
    `(orderless-match-face-0 :foreground ,(doom-color 'magenta) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-1 :foreground ,(doom-color 'yellow) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-2 :foreground ,(doom-color 'cyan) :bold t :background ,(doom-color 'base0))
    `(orderless-match-face-3 :foreground ,(doom-color 'green) :bold t :background ,(doom-color 'base0))))

(use-package! prescient
  :after selectrum
  :config
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
    [remap goto-line] #'consult-goto-line
    [remap imenu] #'consult-imenu
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    [remap man] #'consult-man
    [remap yank-pop] #'consult-yank-pop
    [remap locate] #'consult-locate
    [remap load-theme] #'consult-theme
    [remap recentf-open-files] #'consult-recent-file)

  :config
  ;; Don't be so aggresive with previews
  (setq consult-preview-key (kbd "C-."))

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
  (setq consult-narrow-key (kbd "C->"))

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

  ;; Use expanded file name to compare with project root
  (setq consult--source-project-file
        `(:name      "Project Recent File"
          :narrow    (?p . "Project")
          :category  file
          :face      consult-file
          :history   file-name-history
          :open      ,#'consult--open-file
          :predicate ,(lambda () consult-project-root-function)
          :items
          ,(lambda ()
             (when-let (root (funcall consult-project-root-function))
               (let ((len (length root))
                     (inv-root (propertize root 'invisible t))
                     (ht (consult--cached-buffer-file-hash)))
                 (mapcar (lambda (x)
                           (concat inv-root (substring x len)))
                         (seq-filter (lambda (x)
                                       (and (not (gethash x ht))
                                            (string-prefix-p root x)))
                                     (mapcar #'expand-file-name recentf-list))))))))

  ;; Integrate with evil jumping
  (after! evil
    (evil-set-command-property 'consult-imenu :jump t)
    (evil-set-command-property 'consult-outline :jump t)
    (evil-set-command-property 'consult-mark :jump t)
    (evil-set-command-property 'consult-line :jump t)
    (evil-set-command-property 'consult-line-symbol-at-point :jump t)
    (evil-set-command-property 'consult-line-from-isearch :jump t))

  ;; Better than `org-set-tags-command'
  (after! org
      (defun my/consult-org-set-tags ()
        "Select tags to add to or remove from a headline.
  Choose one or more tags. Chosen tags that are already on the current headline will
  be removed. Chosen tags which are not, will be added."
        (interactive)
        (require 'org)
        (unless (org-at-heading-p) (user-error "Not a headline."))
        (let* ((current (org-get-tags (point)))
               (selected (thread-last (org-get-buffer-tags)
                           (completing-read-multiple "Select org tag(s): "))))
          (org-set-tags
           (seq-uniq (append (seq-difference current selected)
                             (seq-difference selected current))))))
      (defalias #'org-set-tags-command #'my/consult-org-set-tags))

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
  (defun +compres/embark-export-prj-dired (files)
    "Create a dired buffer listing FILES in the current project root."
    (setq files (mapcar #'directory-file-name files))
    (when (dired-check-switches dired-listing-switches "A" "almost-all")
      (setq files (cl-remove-if-not
                   (lambda (path)
                     (let ((file (file-name-nondirectory path)))
                       (unless (or (string= file ".") (string= file ".."))
                         path)))
                   files)))
    (dired (cons (doom-project-root) files))
    (rename-buffer (format "*Embark Export Project Dired %s*" default-directory)))

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
  (defun +compres/resolve-project-file (file)
    "Resolve a file using the project path as a prefix"
    (let* ((project-file (doom-path (doom-project-root) file))
           (target (or (and (file-exists-p project-file)
                            project-file)
                       file)))
      target))
  (defun +compres/prj-find-file (file)
    (interactive "FFile:")
    (find-file (+compres/resolve-project-file file)))
  (defun +compres/prj-find-file-other-window (file)
    (interactive "FFile:")
    (find-file-other-window (+compres/resolve-project-file file)))
  (defun +compres/prj-delete-file (file)
    (interactive "FFile:")
    (delete-file (+compres/resolve-project-file file)))
  (defun +compres/prj-rename-file (file newname)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Rename %s to file:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (rename-file (+compres/resolve-project-file file) newname))
  (defun +compres/prj-copy-file (file newname)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Copy %s to file:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (copy-file (+compres/resolve-project-file file) newname))
  (defun +compres/prj-ediff-files (file-a file-b)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Compare %s to:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (ediff-files (+compres/resolve-project-file file-a) file-b))
  (defun +compres/prj-embark-insert-relative-path (file)
    (interactive "FFile:")
    (embark-insert-relative-path (+compres/resolve-project-file file)))
  (defun +compres/prj-embark-save-relative-path (file)
    (interactive "FFile:")
    (embark-save-relative-path (+compres/resolve-project-file file)))

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
