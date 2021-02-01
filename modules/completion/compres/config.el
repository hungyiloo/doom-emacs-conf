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

  (after! org
    ;; Better selectrum integration with `org-set-tags-command'
    (defun +compres/org-set-tags-command-multiple (orig &optional arg)
      (cl-letf (((symbol-function #'completing-read)
                 (lambda (prompt collection &optional predicate require-match initial-input
                            hist def inherit-input-method)
                   (when initial-input
                     (setq initial-input
                           (replace-regexp-in-string
                            ":" ","
                            (replace-regexp-in-string
                             "\\`:" "" initial-input))))
                   (let ((res (completing-read-multiple
                               prompt collection predicate require-match initial-input
                               hist def inherit-input-method)))
                     (mapconcat #'identity res ":")))))
        (let ((current-prefix-arg arg))
          (call-interactively orig))))
    (advice-add #'org-set-tags-command :around #'+compres/org-set-tags-command-multiple)))

(use-package! orderless
  :after selectrum
  :config
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
  (after! orderless
    ;; Orderless needs to be set up with selectrum after enabling selectrum-prescient-mode.
    ;; Doing this lets us use orderless for filtering and prescient for sorting candidates
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

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

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  ;;(fset 'projectile-ripgrep 'consult-ripgrep)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Don't be so aggresive with previews
  (setq consult-preview-key (kbd "C-."))

  ;; Ensure consult-recent-file returns a list of files on startup.
  ;; Without this, sometimes it can be empty because it hasn't been loaded yet?
  ;; Not sure how to more elgantly trigger a load.
  (recentf-load-list)

  ;; Optionally configure a function which returns the project root directory
  (setq consult-project-root-function
        (defun +compres/get-project-root ()
          (when (fboundp #'project-root)
            (expand-file-name (project-root (project-current))))))

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
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  (setq consult--source-project-buffer
        `(:name      "Project Buffer"
          :narrow    (?p . "Project")
          :category  buffer
          :face      consult-buffer
          :history   buffer-name-history
          :open      ,#'consult--open-buffer
          :predicate ,(lambda () consult-project-root-function)
          :items
          ,(lambda ()
             (when-let (root (funcall consult-project-root-function))
               (mapcar #'buffer-name
                       (seq-filter (lambda (x)
                                     (when-let (file (buffer-file-name x))
                                       (string-prefix-p root file)))
                                   (consult--cached-buffers)))))))

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
    (evil-set-command-property 'consult-line-from-isearch :jump t)))

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

  (setq marginalia-command-categories
        '((imenu . imenu)
          ;; Classify project file choosers as 'project-file'
          (project-find-file . project-file)
          ;; But switching projects are just regular files/dirs
          (project-switch-project . file)))
)

(use-package! embark
  :bind
  ("<f5>" . embark-act)

  :config

  ;; (add-hook! 'embark-target-finders
  ;;   (defun current-candidate+category ()
  ;;     (when (bound-and-true-p selectrum-active-p)
  ;;       (cons (selectrum--get-meta 'category)
  ;;             (selectrum-get-current-candidate)))))

  ;; (add-hook! 'embark-candidate-collectors
  ;;   (defun current-candidates+category ()
  ;;     (when (bound-and-true-p selectrum-active-p)
  ;;       (cons (selectrum--get-meta 'category)
  ;;             (selectrum-get-current-candidates
  ;;              ;; Pass relative file names for dired.
  ;;              minibuffer-completing-file-name)))))

  ;; The following is not selectrum specific but included here for convenience.
  ;; If you don't want to use which-key as a key prompter skip the following code.
  ;; (setq embark-action-indicator
  ;;       (lambda (map) (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;         #'which-key--hide-popup-ignore-command)
  ;;       embark-become-indicator embark-action-indicator)

  ;; FIXME: Ensure selectrum candidates are refreshed after embark act without exit (using C-u)
  ;; (add-hook! #'embark-post-action-hook #'consult-selectrum--refresh)

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
    (let* ((project-file (doom-path (concat (project-root (project-current))) file))
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
    (embark-save-relative-path (+compres/resolve-project-file file))))

(use-package! embark-consult
  :after (embark consult))

;; Better xref experience in absence of ivy
(after! xref
  (set-popup-rule!
    "^\\*xref"
    :size 20
    :quit t)
  (custom-set-faces!
    `(xref-match :foreground ,(doom-color 'magenta) :bold t :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.3))))
