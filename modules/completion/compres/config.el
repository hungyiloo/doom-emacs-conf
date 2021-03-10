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

  ;; (after! org
  ;;   ;; Better selectrum integration with `org-set-tags-command'
  ;;   ;; but the replacement command `+compres/consult-org-set-tags' below might be better
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

  ;; Better minibuffer prepopulation using M-n
  (setq minibuffer-default-add-function
        (defun +compres/minibuffer-default-add-function ()
          (autoload 'ffap-guesser "ffap")
          (with-selected-window (minibuffer-selected-window)
            (delete-dups
             (delq nil
                   (list (thing-at-point 'symbol)
                         (thing-at-point 'list)
                         (ffap-guesser)
                         (thing-at-point-url-at-point)))))))


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
  (defun +compres/consult-line-dwim ()
    "Conduct a text search on the current buffer.
If a selection is active, pre-fill the prompt with it."
    (interactive)
    (if (doom-region-active-p)
        (consult-line (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-line)))

  (defun +compres/consult-line-symbol-at-point ()
    "Conduct a text search on the current buffer for the symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun +compres/consult-ripgrep-dwim (dir)
    "Conduct a text search on in the current (project) directory.
If a selection is active, pre-fill the prompt with it."
    (interactive "P")
    (if (doom-region-active-p)
        (consult-ripgrep dir (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-ripgrep dir)))

  (defun +compres/consult-ripgrep-other-project-dwim (dir)
    "Conduct a text search on in the current (project) directory.
If a selection is active, pre-fill the prompt with it."
    (interactive "P")
    (let ((default-directory
            (if-let (projects (projectile-relevant-known-projects))
                (completing-read "Search project: " projects nil t)
              (user-error "There are no known projects")))
          (this-command #'+compres/consult-ripgrep-other-project-dwim))
      (message "%s" this-command)
      (if (doom-region-active-p)
          (consult-ripgrep dir (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
        (consult-ripgrep dir))))

  (defun +compres/consult-ripgrep-cwd ()
    (interactive)
    (consult-ripgrep default-directory))

  (defun +compres/consult-ripgrep-other-cwd ()
    (interactive)
    (consult-ripgrep t))

  (defun +compres/consult-ripgrep-notes ()
    (interactive)
    (unless (bound-and-true-p org-directory)
      (require 'org))
    (consult-ripgrep org-directory))

  (defun +compres/consult-ripgrep-symbol-at-point (dir)
    "Conduct a text search on in the current (project) directory for the symbol at point."
    (interactive "P")
    (consult-ripgrep dir (thing-at-point 'symbol)))

  (defun +compres/consult-ripgrep-notes-symbol-at-point (dir)
    "Conduct a text search on in the current (project) directory for the symbol at point."
    (interactive "P")
    (unless (bound-and-true-p org-directory)
      (require 'org))
    (consult-ripgrep org-directory (thing-at-point 'symbol)))

  (defun +compres/consult-project-buffer ()
    (interactive)
    (when (doom-project-p)
      (run-at-time 0 nil #'execute-kbd-macro (kbd "p SPC"))) ; this feels DIRTY but it works
    (consult-buffer))

  (defun +compres/consult-find-under-here (dir)
    (interactive "P")
    (consult-find (or dir default-directory)))

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
    (evil-set-command-property #'consult-imenu :jump t)
    (evil-set-command-property #'consult-outline :jump t)
    (evil-set-command-property #'consult-mark :jump t)
    (evil-set-command-property #'consult-line :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-cwd :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-dwim :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-other-cwd :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-symbol-at-point :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-other-project-dwim :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-notes :jump t)
    (evil-set-command-property #'+compres/consult-line-dwim :jump t)
    (evil-set-command-property #'+compres/consult-line-symbol-at-point :jump t)
    (evil-set-command-property #'+compres/consult-file-under-here :jump t)
    (evil-set-command-property #'+compres/consult-ripgrep-notes-symbol-at-point :jump t))

  ;; Better than `org-set-tags-command'
  (after! org
    (defun +compres/consult-org-set-tags ()
      "Select tags to add to or remove from a headline.
  Choose one or more tags. Chosen tags that are already on the current headline will
  be removed. Chosen tags which are not, will be added."
      (interactive)
      (require 'org)
      (when (org-before-first-heading-p) (user-error "Not a headline."))
      (save-excursion
        (org-back-to-heading)
        (let* ((current (org-get-tags (point)))
               (selected (thread-last (org-get-buffer-tags)
                           (completing-read-multiple "Select org tag(s): "))))
          (org-set-tags
           (seq-uniq (append (seq-difference current selected)
                             (seq-difference selected current)))))))
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

(after! ispell
  (defun +spell/correct ()
    "Correct spelling of word at point."
    (interactive)
    ;; spell-fu fails to initialize correctly if it can't find aspell or a similar
    ;; program. We want to signal the error, not tell the user that every word is
    ;; spelled correctly.
    (unless (;; This is what spell-fu uses to check for the aspell executable
             or (and ispell-really-aspell ispell-program-name)
             (executable-find "aspell"))
      (user-error "Aspell is required for spell checking"))

    (ispell-set-spellchecker-params)
    (save-current-buffer
      (ispell-accept-buffer-local-defs))
    (cl-destructuring-bind (start . end)
        (or (bounds-of-thing-at-point 'word)
            (user-error "No word at point"))
      (let ((word (thing-at-point 'word t))
            (orig-pt (point))
            poss ispell-filter)
        (ispell-send-string "%\n")
        (ispell-send-string (concat "^" word "\n"))
        (while (progn (accept-process-output ispell-process)
                      (not (string= "" (car ispell-filter)))))
        ;; Remove leading empty element
        (setq ispell-filter (cdr ispell-filter))
        ;; ispell process should return something after word is sent. Tag word as
        ;; valid (i.e., skip) otherwise
        (unless ispell-filter
          (setq ispell-filter '(*)))
        (when (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
        (cond
         ((or (eq poss t) (stringp poss))
          ;; don't correct word
          (message "%s is correct" (funcall ispell-format-word-function word))
          t)
         ((null poss)
          ;; ispell error
          (error "Ispell: error in Ispell process"))
         (t
          ;; The word is incorrect, we have to propose a replacement.
          (setq res (funcall +spell-correct-interface (nth 2 poss) word))
          ;; Some interfaces actually eat 'C-g' so it's impossible to stop rapid
          ;; mode. So when interface returns nil we treat it as a stop.
          (unless res (setq res (cons 'break word)))
          (cond
           ((stringp res)
            (+spell--correct res poss word orig-pt start end))
           ((let ((cmd (car res))
                  (wrd (cdr res)))
              (unless (or (eq cmd 'skip)
                          (eq cmd 'break)
                          (eq cmd 'stop))
                (+spell--correct cmd poss wrd orig-pt start end)
                (unless (string-equal wrd word)
                  (+spell--correct wrd poss word orig-pt start end))))))
          (ispell-pdict-save t)))))))
