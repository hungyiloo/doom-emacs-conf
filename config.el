;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hung-Yi Loo"
      user-mail-address "hungyi.loo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-material)

;; Set a custom font
(setq doom-font (font-spec :family "JetBrains Mono Semi Light" :size 16))

;; Better default window placement on startup
(setq initial-frame-alist '((width . 141) (height . 45) (fullscreen . fullheight)))

;; Always revert files automatically
(global-auto-revert-mode 1)

(use-package! magit
  :hook (magit-mode . my-magit-fringes)
  :config
  ;; Wider fringe (emacs default) for better magit support
  (defun my-magit-fringes ()
    (setq left-fringe-width 20
          right-fringe-width 0)))

(use-package! olivetti
  :hook (text-mode . olivetti-mode)
  :init
  (setq olivetti-body-width 80))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; Make sure org supports org-id stuff
(add-to-list 'org-modules 'org-id)

(after! org
  ;; Log CLOSED timestamp when notes are set to DONE state
  (setq org-log-done 'time)
  ;; Show more whitespace in org mode when cycling
  (setq org-cycle-separator-lines -1)
  ;; Always use ID properties to store links
  (setq org-id-link-to-org-use-id 'use-existing)
  ;; Set custom header bullets
  ;; (setq org-superstar-headline-bullets-list '("★" "▸" "◼" "⚫" "·"))
  ;; (setq org-superstar-cycle-headline-bullets nil)
  ;; Don't export org files with table of contents by default
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  ;; When storing links by ID, add them to the normal `org-stored-links' variable
  (defadvice! +org--store-id-link-a (link)
    :filter-return #'org-id-store-link
    (when (and link org-store-link-plist)
      (add-to-list 'org-stored-links
                   (list (plist-get org-store-link-plist :link)
                         (plist-get org-store-link-plist :description))))
    link)
  ;; A function to copy the URL from an org mode link
  (defun my-org-retrieve-url-from-point ()
    "Copies the URL from an org link at the point"
    (interactive)
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   ;; org-context seems to return nil if the current element
                   ;; starts at buffer-start or ends at buffer-end
                   (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                   (or (caddr link-info) (point-max))))))
      (if (not text)
          (error "Oops! Point isn't in an org link")
        (string-match org-link-bracket-re text)
        (kill-new (substring text (match-beginning 1) (match-end 1))))))
  ;; Map `my-org-retrieve-url-from-point' to live with its org link friends
  (map! :map org-mode-map
        :localleader
        (:prefix ("l" . "links")
         "y" #'my-org-retrieve-url-from-point))
  ;; Hide emphasis markers (e.g. italics, bold)
  (setq org-hide-emphasis-markers t))

(use-package! kurecolor
  :config
  ;; Redefine this function.
  ;; The old one used to delete a space before hex colors.
  ;; I think `bounds-of-thing-at-point' for 'symbol already
  ;; accounts for the '#' character at the beginning, so we
  ;; don't need to move back one space for pos1.
  (defun kurecolor-replace-current (fn &rest args)
    "Get the current unspaced string at point.
Replace with the return value of the function FN with ARGS"
    (let (pos1 pos2 replacement excerpt change)
      (if (and transient-mark-mode mark-active)
          (setq pos1 (region-beginning) pos2 (region-end))
        (progn
          (when (looking-at "#") (forward-char 1))
          (setq pos1 (car (bounds-of-thing-at-point 'symbol))
                pos2 (cdr (bounds-of-thing-at-point 'symbol)))))
      (setq excerpt (buffer-substring-no-properties pos1 pos2))
      (if args
          (progn (setq change (car args))
                 (setq replacement (funcall fn excerpt change)))
        ;; no args
        (setq replacement (funcall fn excerpt)))
      (delete-region pos1 pos2)
      (insert replacement)))
  (defun my-kurecolor-open-hydra ()
    "Makes sure hl-line-mode is off and opens the kurecolor hydra"
    (interactive)
    (hl-line-mode -1)
    (funcall #'+rgb/kurecolor-hydra/body))
  (map! :map (css-mode-map sass-mode-map stylus-mode-map)
        :localleader
        (:prefix ("c" . "colors")
         "h" #'hl-line-mode
         "k" #'my-kurecolor-open-hydra)))

(use-package! evil
  :config
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  ;; (setq evil-escape-key-sequence "hl")
  ;; (setq evil-escape-unordered-key-sequence t)
  ;; (setq evil-escape-delay 0.05)
  (setq evil-snipe-scope 'visible))

(after! avy
  ;; Configure avy to use colemak home row
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(after! ivy
  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setq ivy-more-chars-alist '((counsel-grep . 3)
                               (counsel-rg . 3)
                               (counsel-search . 3)
                               (t . 3))))

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-clients-angular-language-server-command
  '("node"
    "/home/hungyi/.config/yarn/global/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/home/hungyi/.config/yarn/global/node_modules"
    "--tsProbeLocations"
    "/home/hungyi/.config/yarn/global/node_modules"
    "--stdio")))

(use-package! ivy-rich
  :init
  (setq
   ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-switch-buffer-transformer
        (:width 60)) ;; Wider than the default
       (ivy-rich-switch-buffer-size
        (:width 7))
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode
        (:width 12 :face warning))
       (ivy-rich-switch-buffer-project
        (:width 15 :face success))
       (ivy-rich-switch-buffer-path
        (:width
         (lambda
           (x)
           (ivy-rich-switch-buffer-shorten-path x
                                                (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda
        (cand)
        (get-buffer cand)))
     counsel-find-file
     (:columns
      ((ivy-read-file-transformer)
       (ivy-rich-counsel-find-file-truename
        (:face font-lock-doc-face))))
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer
        (:width 60))
       (ivy-rich-counsel-function-docstring
        (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer
        (:width 40))
       (ivy-rich-counsel-function-docstring
        (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer
        (:width 40))
       (+ivy-rich-describe-variable-transformer
        (:width 50))
       (ivy-rich-counsel-variable-docstring
        (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-candidate
        (:width 0.8))
       (ivy-rich-file-last-modified-time
        (:face font-lock-comment-face))))
     package-install
     (:columns
      ((ivy-rich-candidate
        (:width 30))
       (ivy-rich-package-version
        (:width 16 :face font-lock-comment-face))
       (ivy-rich-package-archive-summary
        (:width 7 :face font-lock-builtin-face))
       (ivy-rich-package-install-summary
        (:face font-lock-doc-face))))
     counsel-projectile-switch-to-buffer
     (:columns
      ((ivy-switch-buffer-transformer
        (:width 30))
       (ivy-rich-switch-buffer-size
        (:width 7))
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode
        (:width 12 :face warning))
       (ivy-rich-switch-buffer-project
        (:width 15 :face success))
       (ivy-rich-switch-buffer-path
        (:width
         (lambda
           (x)
           (ivy-rich-switch-buffer-shorten-path x
                                                (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda
        (cand)
        (get-buffer cand)))
     counsel-bookmark
     (:columns
      ((ivy-rich-candidate
        (:width 0.5))
       (ivy-rich-bookmark-filename
        (:width 60)))))))


;; (after! neotree
;;   ;; Allow resizing of neotree window
;;   (setq neo-window-fixed-size nil)
;;   ;; Don't reset neotree window size when opening a file
;;   (add-to-list 'window-size-change-functions
;;                (lambda ()
;;                  (let ((neo-window (neo-global--get-window)))
;;                    (unless (null neo-window)
;;                      (setq neo-window-width (window-width neo-window))))))
;;   ;; Keep neotree window size when opening/closing
;;   (defun neo-window--zoom (method)
;;     "Zoom the NeoTree window, the METHOD should one of these options:
;;      'maximize 'minimize 'zoom-in 'zoom-out."
;;     (neo-buffer--unlock-width)
;;     (cond
;;      ((eq method 'maximize)
;;       (maximize-window))
;;      ((eq method 'minimize)
;;       (message "neotree override: its window width will not be reset"))
;;      ((eq method 'zoom-in)
;;       (shrink-window-horizontally 2))
;;      ((eq method 'zoom-out)
;;       (enlarge-window-horizontally 2)))
;;     (neo-buffer--lock-width))
;;   ;; Enable richer icons on neotree
;;   (setq doom-themes-neotree-enable-file-icons t))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Fix some edge case javascript indenting
(after! js2-mode
  (setq js-indent-level 2))
(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq tide-native-json-parsing t)
  (setq tide-completion-ignore-case t))

;; Use 2-space indentation in web-mode always
(after! web-mode
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(add-hook! 'doom-load-theme-hook
  (let* ((bg (doom-color 'bg))
         (darker-bg (doom-darken bg 0.7))
         (color-A (doom-color 'red))
         (color-B (doom-color 'green))
         (color-C (doom-color 'teal))
         (current-bg-A (doom-blend color-A darker-bg 0.1))
         (current-bg-B (doom-blend color-B darker-bg 0.115))
         (current-bg-C (doom-blend color-C darker-bg 0.115))
         (current-bg-fine-A (doom-blend color-A bg 0.15))
         (current-bg-fine-B (doom-blend color-B bg 0.15))
         (current-bg-fine-C (doom-blend color-C bg 0.25))
         (other-bg-A (doom-blend color-A bg 0.05))
         (other-bg-B (doom-blend color-B bg 0.05))
         (other-bg-C (doom-blend color-C bg 0.05)))
    (custom-set-faces!
      ;; Make tab bar background transparent so that it matches the theme
      '(tab-line :inherit variable-pitch :foreground "black" :height 0.9)
      ;; Customize material cursor color to not be so garish
      ;; Also so that it doesn't conflict with the mc/multiedit cursors
      `(cursor :background ,(doom-color 'dark-cyan))
      ;; Customize ediff highlighting
      `(ediff-fine-diff-A    :background ,current-bg-fine-A :weight unspecified :extend t)
      `(ediff-fine-diff-B    :background ,current-bg-fine-B :weight unspecified :extend t)
      `(ediff-fine-diff-C    :background ,current-bg-fine-C :weight unspecified :extend t)
      `(ediff-current-diff-A :background ,current-bg-A :extend t)
      `(ediff-current-diff-B :background ,current-bg-B :extend t)
      `(ediff-current-diff-C :background ,current-bg-C :extend t)
      `(ediff-even-diff-A    :background ,other-bg-A :extend t)
      `(ediff-even-diff-B    :background ,other-bg-B :extend t)
      `(ediff-even-diff-C    :background ,other-bg-C :extend t)
      `(ediff-odd-diff-A     :background ,other-bg-A :extend t)
      `(ediff-odd-diff-B     :background ,other-bg-B :extend t)
      `(ediff-odd-diff-C     :background ,other-bg-C :extend t))))

(add-hook! 'ediff-prepare-buffer-hook #'solaire-mode)

;; Allow links to be opened outside WSL
(when (and (eq system-type 'gnu/linux)
           (string-match "Linux.*Microsoft.*Linux" (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function 'browse-url-generic))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Temporary fixes below
;; They should be reviewed regularly to see if they are still needed

;; DEPRECATED: Check if the original implementation at
;; ~/.emacs.d/modules/config/default/autoload/files.el
;; is fixed or not. Does it still throw an error "stringp nil"?
;; Seems to be fixed here: https://github.com/hlissner/doom-emacs/commit/9c96feba56ff687f2c8376bfe76363a53bf601ec
;; (defun +default/dired (arg)
;;   "Open a directory in dired.
;; If prefix ARG is non-nil, prompt for a known project to open in dired."
;;   (interactive "P")
;;   (if arg
;;       (dired (completing-read "Open dired in project: " projectile-known-projects))
;;     ;; This line is the key to the fix.
;;     ;; `dired-read-dir-and-switches' returns a list, but dired doesn't take a list
;;      (apply #'dired (dired-read-dir-and-switches ""))))

;; DEPRECATED: Temporarily disable eldoc mode in org mode due to
;; bugs in Emacs 28. Maybe this isn't needed anymore?
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg129389.html
;; (add-hook! 'org-mode-hook
;;   (funcall eldoc-mode -1))

;; Fixing eldoc issues in tide with emacs 28
;; PR already in tide -- might not be needed once doom bumps
;; (when (< 28 emacs-major-version)
;;   (defalias 'eldoc-display-message-no-interference-p 'eldoc-display-message-p))
;; (add-hook! 'tide-mode-hook
;;   ;;; Copied from eldoc.el
;;   (defun tide-eldoc-maybe-show (text)
;;     (with-demoted-errors "eldoc error: %s"
;;       (and (or (eldoc-display-message-no-interference-p)
;;               ;; Erase the last message if we won't display a new one.
;;               (when eldoc-last-message
;;                 (eldoc-message nil)
;;                 nil))
;;           (eldoc-message text)))))
