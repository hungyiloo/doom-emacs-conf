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
(setq doom-theme 'doom-miramare)
;; (load-theme 'doom-miramare t)

;; Set a custom font
(setq doom-font (font-spec :family "JetBrains Mono Semi Light" :size 15)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 15))

;; Better default window placement on startup
(setq initial-frame-alist '((width . 141) (height . 45) (fullscreen . fullheight)))

;; Font adjustments should be more fine
(setq text-scale-mode-step 1.05)
(setq-default line-spacing 1)

;; Always revert files automatically
(global-auto-revert-mode 1)

;; Load some external files
(load! "lisp/hydras.el")
(load! "lisp/journal.el")
(load! "lisp/colors.el")

(use-package! magit
  :hook (magit-mode . my-magit-fringes)
  :config
  ;; Wider fringe (emacs default) for better magit support
  (defun my-magit-fringes ()
    (setq left-fringe-width 20
          right-fringe-width 0)))

(use-package! olivetti
  :hook (org-mode . olivetti-mode)
  :hook (markdown-mode . olivetti-mode)
  :hook (olivetti-mode . my-olivetti-setup)
  :init
  (setq olivetti-body-width 80)
  (defun my-olivetti-setup ()
    (setq doom--line-number-style nil)
    (setq display-line-numbers nil)))

;; (use-package! writeroom-mode
;;   :config
;;   (setq writeroom-width 50)
;;   (setq +zen-text-scale 1)
;;   (setq +zen-window-divider-size 1))

;; (use-package mixed-pitch
;;   :hook (mixed-pitch-mode . my-mixed-pitch-setup)
;;   :config
;;   (defun my-mixed-pitch-setup ()
;;     (if mixed-pitch-mode
;;         (progn (set-face-attribute 'org-hide nil :inherit '(fixed-pitch))
;;                (set-face-attribute 'org-superstar-leading nil :inherit '(fixed-pitch))
;;                (set-face-attribute 'org-superstar-header-bullet nil :inherit '(fixed-pitch))
;;                (setq line-spacing 6))
;;       (progn
;;         (set-face-attribute 'org-verbatim nil :font (font-spec :family "JetBrains Mono NL"))
;;         (setq line-spacing nil))))
;;   (pushnew! mixed-pitch-fixed-pitch-faces
;;             'outline-1
;;             'outline-2
;;             'outline-3
;;             'outline-4
;;             'outline-5
;;             'outline-6
;;             'outline-7
;;             'outline-8))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")


(use-package! org
  :init
  ;; Set custom header bullets
  (setq org-superstar-headline-bullets-list '("•"))
  (setq org-superstar-cycle-headline-bullets nil)
  ;; Show more whitespace in org mode when cycling
  (setq org-cycle-separator-lines -1)
  ;; Make sure org supports org-id stuff
  (add-to-list 'org-modules 'org-id)
  :config
  ;; Log CLOSED timestamp when notes are set to DONE state
  (setq org-log-done 'time)
  ;; Always use ID properties to store links
  (setq org-id-link-to-org-use-id 'use-existing)
  ;; Don't export org files with table of contents by default
  (setq org-export-with-toc nil)
  ;; Don't export section numbers
  (setq org-export-with-section-numbers nil)
  ;; Also show state changes (such as finished recurring tasks) in agenda
  (setq org-agenda-log-mode-items '(closed clock state))
  ;; Allow large tables to be processed
  (setq org-table-convert-region-max-lines 9999)
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
    (let ((plain-url (url-get-url-at-point)))
      (if plain-url
          (progn
            (kill-new plain-url)
            (message (concat "Copied: " plain-url)))
        (let* ((link-info (assoc :link (org-context)))
               (text (when link-info
                       ;; org-context seems to return nil if the current element
                       ;; starts at buffer-start or ends at buffer-end
                       (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                       (or (caddr link-info) (point-max))))))
          (if (not text)
              (error "Oops! Point isn't in an org link")
            (string-match org-link-bracket-re text)
            (let ((url (substring text (match-beginning 1) (match-end 1))))
              (kill-new url)
              (message (concat "Copied: " url))))))))
  (defun org-babel-execute:html (body _params)
    "Execute a block of HTML code.
This function is called by `org-babel-execute-src-block'."
    body)

  ;; Map `my-org-retrieve-url-from-point' to live with its org link friends
  (map! :map org-mode-map
        :localleader
        (:prefix ("l" . "links")
         "y" #'my-org-retrieve-url-from-point)))

(use-package! git-gutter
  :init
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode org-mode))
  :config
  (defun my-org-hook-start-without-vc-gutter ()
    "Set up `git-gutter-mode' in the current buffer regardless of `git-gutter:disabled-modes' and leave it off initially."
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (when (or +vc-gutter-in-remote-files
                (not (file-remote-p (or file-name default-directory))))
        (if (null file-name)
            (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local)
          (when (vc-backend file-name)
            (if (and (display-graphic-p)
                     (require 'git-gutter-fringe nil t))
                (setq-local git-gutter:init-function      #'git-gutter-fr:init
                            git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                            git-gutter:clear-function     #'git-gutter-fr:clear
                            git-gutter:window-width -1)
              (setq-local git-gutter:init-function      'nil
                          git-gutter:view-diff-function #'git-gutter:view-diff-infos
                          git-gutter:clear-function     #'git-gutter:clear-diff-infos
                          git-gutter:window-width 1))
            (git-gutter-mode -1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))
  (add-hook! 'org-mode-hook #'my-org-hook-start-without-vc-gutter))

(map! :leader
      "<right>" #'better-jumper-jump-forward
      "<left>" #'better-jumper-jump-backward
      (:prefix-map ("=" . "calc")
       "=" #'calc-dispatch
       "c" #'calc
       "q" #'quick-calc
       "g" #'calc-grab-region)
      (:prefix-map ("t" . "toggle")
       :desc "Git gutter" "v" #'git-gutter-mode
       :desc "Highlight line" "h" #'hl-line-mode)
      (:prefix-map ("p" . "project")
       "v" #'projectile-run-vterm))

(map! :map evil-window-map
      ;; Use the normal other-window command
      ;; to take advantage of the window-select module
      "w" #'other-window
      "~" #'ace-swap-window)

;; (map! :after dired
;;       :map dired-mode-map
;;       :n "RET" #'dired-find-alternate-file
;;       :desc "dired-up-directory (alt)" :n "^" (lambda () (interactive) (find-alternate-file "..")))

(use-package! evil
  :config
  (setq evil-shift-width 2)
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  ;; (setq evil-escape-key-sequence "hl")
  ;; (setq evil-escape-unordered-key-sequence t)
  ;; (setq evil-escape-delay 0.05)
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-keys nil)
  (evil-set-initial-state 'vterm-mode 'normal)
  (map!
   :i "C-S-SPC" #'hippie-expand
   (:when (featurep! :editor multiple-cursors)
    :prefix "g"
    :nv "z" #'my-mc-hydra/body)
   :prefix "gs"
   :nv "p" #'my-sp-hydra/body)
  ;; Fix ^ movement to also respect visual line mode
  (when evil-respect-visual-line-mode
    (evil-define-minor-mode-key 'motion 'visual-line-mode
      "^"  #'evil-first-non-blank-of-visual-line
      "g^" #'evil-first-non-blank)))

(use-package! evil-collection
  :config
  ;; Fix regular linewise movement in org mode and outline mode.
  ;; Previously they were mapped to `outline-backward-same-level' and `outline-forward-same-level'.
  ;; But in org mode (where this matters) it's available via '[h' and ']h' already.
  ;; No need for gk and gj to be remapped.
  (evil-collection-define-key 'normal 'outline-mode-map
    "gk" nil
    "gj" nil))

(after! evil-goggles
  (setq evil-goggles-duration 0.2))

(after! avy
  ;; Configure avy to use colemak home row
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(after! ace-window
  (setq aw-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(after! ivy
  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setq ivy-more-chars-alist '((counsel-grep . 3)
                               (counsel-rg . 3)
                               (counsel-search . 3)
                               (t . 3))))

(use-package! swiper
  :config
  ;; Advise `swiper-isearch' to use `rxt-pcre-to-elisp' and `rxt-quote-pcre' so that
  ;; SPC s s and SPC s S correctly deals with regex sensitive
  ;; characters in the selected region or symbol at point
  (defun my-rxt-quoted-swiper-isearch (orig-fun &rest args)
    (interactive)
    (apply orig-fun (mapcar (lambda (x) (rxt-pcre-to-elisp (rxt-quote-pcre x))) args)))
  (advice-add #'swiper-isearch :around #'my-rxt-quoted-swiper-isearch))

(use-package! counsel
  :config
  (setq counsel-search-engine 'google)
  ;; Advise `counsel-rg' to use `rxt-pcre-to-elisp' so that
  ;; SPC s p correctly deals with regex sensitive
  ;; characters in the selected region or symbol at point
  (defun my-rxt-elisp-counsel-rg (orig-fun &rest args)
    (interactive)
    (if (> 0 (length args))
        (apply orig-fun (cons (rxt-pcre-to-elisp (car args)) (cdr args)))
      (apply orig-fun args)))
  (advice-add #'counsel-rg :around #'my-rxt-elisp-counsel-rg))

(after! lsp-mode
  ;; Setting this disables DOOM's deferred shutdown functionality.
  (setq +lsp-defer-shutdown nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/home/hungyi/.config/yarn/global/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/home/hungyi/.config/yarn/global/node_modules"
          "--tsProbeLocations"
          "/home/hungyi/.config/yarn/global/node_modules"
          "--stdio"))
  (setq lsp-clients-typescript-server-args
        '("--stdio"
          "--tsserver-path=/home/hungyi/.yarn/bin/tsserver")))

(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map)))

(use-package! titlecase
  :load-path "lisp"
  :config
  (setq titlecase-command (concat doom-private-dir "bin/titlecase")))

(use-package! ivy-rich
  :after ivy
  :config
  (let* ((ivy-switch-buffer-plist (plist-get
                                   ivy-rich-display-transformers-list
                                   'ivy-switch-buffer))
         (column-config (plist-get
                         ivy-switch-buffer-plist
                         :columns)))
    (plist-put!
     ivy-switch-buffer-plist
     :columns
     (cons '(ivy-switch-buffer-transformer (:width 60)) (cdr column-config))))
  (ivy-rich-mode -1)
  (ivy-rich-mode +1))

(setq display-line-numbers-type t)

;; Fix some edge case javascript indenting
(after! js2-mode
  (setq js-indent-level 2))
(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq tide-native-json-parsing t)
  (setq tide-completion-ignore-case t))

;; Use 2-space indentation in web-mode always
(use-package! web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset nil)
  (setq emmet-indentation 2)
  ;; Redefine this function to fix incomplete tag renames
  (defun web-mode-element-rename (&optional tag-name)
    "Rename the current html element."
    (interactive)
    (save-excursion
      (let (pos)
        (unless tag-name (setq tag-name (read-from-minibuffer "New tag name? ")))
        (when (and (> (length tag-name) 0)
                   (web-mode-element-beginning)
                   ;; Changed this from ? to * ------------------------v
                   (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)"))
          (setq pos (point))
          (unless (web-mode-element-is-void)
            (save-match-data
              (web-mode-tag-match)
              ;; Changed this from ? to * ---------------------------------v
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)")
                  (replace-match (concat "</" tag-name))
                )))
          (goto-char pos)
          (replace-match (concat "<" tag-name))
          )))))

(use-package! editorconfig
  :config
  (setcdr (assq 'web-mode editorconfig-indentation-alist)
          '((web-mode-indent-style lambda (size) 2)
            ;; I prefer the web mode attr indent behavior when it's set to nil
            ;;
            ;; <a href="http://google.com"
            ;;    target="_blank">See how the attributes line up vertically?</a>
            ;; 
            ;; web-mode-attr-indent-offset
            ;; web-mode-attr-value-indent-offset

            web-mode-code-indent-offset
            web-mode-css-indent-offset
            web-mode-markup-indent-offset
            web-mode-sql-indent-offset
            web-mode-block-padding
            web-mode-script-padding
            web-mode-style-padding)))

;;Use 2-space indentation in css
(after! css-mode
  (setq css-indent-offset 2))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (pushnew! tree-sitter-major-mode-language-alist
            '(scss-mode . css)))

(after! scss-mode
  (defun my-scss-mode-setup ()
    (setq comment-start "/* "
          comment-end " */"))
  (add-hook! 'scss-mode-hook #'my-scss-mode-setup))

(use-package! treemacs
  :init
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  :config
  (setq treemacs-wrap-around nil))

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
         (other-bg-C (doom-blend color-C bg 0.05))
         (treemacs-face-height 95)
         ;; (clearer-region (doom-blend (doom-color 'base4) (doom-color 'base3) 0.1))
         )
    (custom-set-faces!
      ;; Make tab bar background transparent so that it matches the theme
      ;; '(tab-line :inherit variable-pitch :foreground "black" :height 0.9)

      ;; Customize material cursor color to not be so garish
      ;; Also so that it doesn't conflict with the mc/multiedit cursors
      ;; `(cursor :background ,(doom-color 'dark-cyan))

      ;; These might make comments clearer in miramare?
      ;; `(font-lock-comment-face :foreground ,(doom-color 'base5))
      ;; `(magit-hash :foreground ,(doom-color 'base5))

      ;; More visible region background faces
      ;; `(region :background ,clearer-region)
      ;; `(evil-mc-region-face :background ,clearer-region)

      ;; Better styling for parentheses matching
      `(show-paren-match :background unspecified :foreground "#ff0000" :underline t)

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
      `(ediff-odd-diff-C     :background ,other-bg-C :extend t)

      ;; Ignore ligatures in org-verbatim and org-code
      `(org-verbatim :font ,(font-spec :family "JetBrains Mono NL" :extend t))
      `(org-code     :font ,(font-spec :family "JetBrains Mono NL" :extend t))

      ;; Fix tree-sitter punctuation having stuck bg color
      `(tree-sitter-hl-face:punctuation :inherit unspecified)

      ;; Treemacs customizations
      `(treemacs-root-face :height 130 :weight light)
      `(treemacs-file-face :height ,treemacs-face-height)
      `(treemacs-git-modified-face :height ,treemacs-face-height)
      `(treemacs-git-conflict-face :height ,treemacs-face-height)
      `(treemacs-git-untracked-face :height ,treemacs-face-height :foreground ,(doom-color 'blue))
      `(treemacs-git-ignored-face :height ,treemacs-face-height)
      `(treemacs-git-renamed-face :height ,treemacs-face-height)
      `(treemacs-git-unmodified-face :height ,treemacs-face-height)
      `(treemacs-directory-face :height ,treemacs-face-height :weight normal)
      ;; `(doom-themes-treemacs-file-face :height unspecified)
      ;; `(doom-themes-treemacs-root-face :height unspecified :weight unspecified)
      ))
  (setq +ligatures-extra-symbols
    '(;; org
      :name          "»"
      :src_block     "»"
      :src_block_end "«"
      :quote         "“"
      :quote_end     "”"
      ;; Functional
      :lambda        "λ"
      :def           "ƒ"
      :composition   "○"
      :map           "→"
      ;; Types
      :null          "∅"
      :true          "⊤"
      :false         "⊥"
      :int           "ℤ"
      :float         "ℝ"
      :str           "ℂ"
      :bool          "𝔹"
      :list          "ℓ"
      ;; Flow
      :not           "¬"
      :in            "∈"
      :not-in        "∉"
      :and           "∧"
      :or            "∨"
      :for           "∀"
      :some          "∃"
      :return        "⟼"
      :yield         "⟻"
      ;; Other
      :union         "∪"
      :intersect     "∩"
      :diff          "∖"
      :tuple         "⨂"
      :pipe          "║"
      :dot           "•"))
  (set-fontset-font t ?» (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?» (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?« (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?“ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?” (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?λ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?ƒ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∅ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⊤ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⊥ (font-spec :family "Fira Code"))
  (set-fontset-font t ?ℤ (font-spec :family "Fira Code"))
  (set-fontset-font t ?ℝ (font-spec :family "Fira Code"))
  (set-fontset-font t ?𝔹 (font-spec :family "Fira Code"))
  (set-fontset-font t ?ℂ (font-spec :family "Fira Code"))
  (set-fontset-font t ?𝔹 (font-spec :family "Fira Code"))
  (set-fontset-font t ?∈ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∉ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∧ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∨ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∀ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∃ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?⟼ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⟻ (font-spec :family "Fira Code"))
  (set-fontset-font t ?∪ (font-spec :family "Free Mono"))
  (set-fontset-font t ?∩ (font-spec :family "Free Mono"))
  (set-fontset-font t ?∖ (font-spec :family "Free Mono"))
  (set-fontset-font t ?⨂ (font-spec :family "Free Mono"))
  (set-fontset-font t ?• (font-spec :family "JetBrains Mono")))

(use-package! emojify
  :config
  ;; I created a folder ~/.emacs.d/emojis/twemoji-latest and
  ;; downloaded the PNG assets from https://github.com/twitter/twemoji
  (setq emojify-emoji-set "twemoji-latest")
  (setq emojify-emoji-json (concat doom-private-dir "emoji.json")))

;; Include ediff buffers in solaire-mode so they look the same
;; as regular editing buffers
(add-hook! 'ediff-prepare-buffer-hook #'solaire-mode)

;; Allow links to be opened outside WSL
(when (and (eq system-type 'gnu/linux)
           (string-match "Linux.*Microsoft.*Linux" (shell-command-to-string "uname -a")))
  (defun my-browse-url-generic-wsl-safe (url &optional new-window)
    (interactive)
    (let ((parsed-url (thread-last
                          url
                        (replace-regexp-in-string "file://" "file://wsl%24/Ubuntu")
                        (replace-regexp-in-string "\\(wsl%24/Ubuntu\\)?/mnt/c/" "C:/")
                        (url-encode-url))))
      (message "%s" (concat "Browsing to: " parsed-url))
      (apply #'browse-url-generic (list parsed-url new-window))))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'my-browse-url-generic-wsl-safe)
  (after! org
    ;; Make sure org export opens things in the right directory
    (setq org-file-apps '((auto-mode . emacs)
                          (directory . emacs)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . (lambda (_file link) (my-browse-url-generic-wsl-safe link)))
                          ("\\.pdf\\'" . (lambda (_file link) (my-browse-url-generic-wsl-safe link)))))))

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

