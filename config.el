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
(setq doom-font (font-spec :family "JetBrains Mono Semi Light" :size 16)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 15))

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
  :hook (org-mode . olivetti-mode)
  :hook (markdown-mode . olivetti-mode)
  :init
  (setq olivetti-body-width 80))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; Make sure org supports org-id stuff
(add-to-list 'org-modules 'org-id)

(use-package! org
  :init
  ;; Set custom header bullets
  (setq org-superstar-headline-bullets-list '("•"))
  (setq org-superstar-cycle-headline-bullets nil)
  ;; Show more whitespace in org mode when cycling
  (setq org-cycle-separator-lines -1)
  :hook (org-mode . my-org-hook)
  :config
  ;; Log CLOSED timestamp when notes are set to DONE state
  (setq org-log-done 'time)
  ;; Always use ID properties to store links
  (setq org-id-link-to-org-use-id 'use-existing)
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
    "Execute a block of CSS code.
This function is called by `org-babel-execute-src-block'."
    body)
  (defun my-org-hook ()
    ;; Manually set up git-gutter, but don't enable it
    (+vc-gutter-explicit-init-maybe-h-start-off))
  ;; Map `my-org-retrieve-url-from-point' to live with its org link friends
  (map! :map org-mode-map
        :localleader
        (:prefix ("l" . "links")
         "y" #'my-org-retrieve-url-from-point)))

(use-package! git-gutter
  :init
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode org-mode))
  :config
  (defun +vc-gutter-explicit-init-maybe-h-start-off ()
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
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))

(use-package! kurecolor
  :config

  ;; Redefine this function to specifically detect rgba/hex color "symbols"
  (defun kurecolor-replace-current (fn &rest args)
    "Get the current unspaced string at point. Replace with the return value of the function FN with ARGS."
    (let* ((search-range (max (- (point) (line-beginning-position))
                              (- (line-end-position) (point))))
           (bounds (if (and transient-mark-mode mark-active)
                       (list (region-beginning) (region-end))
                     (when (or (thing-at-point-looking-at
                                "#[0-9a-f]\\{6,8\\}"
                                search-range)
                               (thing-at-point-looking-at
                                "rgba?(\s*\\(,?\s*[0-9]\\{1,3\\}\\)\\{3\\}\\(,\s*[0-9]+\.?[0-9]*\\)?\s*)"
                                search-range))
                       (list (match-beginning 0) (match-end 0)))))
           (excerpt (apply #'buffer-substring-no-properties bounds))
           (change (car args))
           (replacement (if args
                            (funcall fn excerpt change)
                          ;; no args
                          (funcall fn excerpt))))

      (apply #'delete-region bounds)
      (insert replacement)))

  ;; Redefine this function to fully handle rgba
  (defun kurecolor-cssrgb-to-hex (cssrgb)
    "Convert a CSSRGB (or rgba) color to hex."
    (let ((rgb (cdr
                (s-match
                 (concat "rgba?(\s*"
                         "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                         "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                         "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,?\s*"
                         "\\([0-9]\.?[0-9]*\\)?)")
                 cssrgb))))
      (if (= 3 (length rgb))
          (cl-destructuring-bind (r g b) (mapcar 'string-to-number rgb)
            (format "#%02X%02X%02X" r g b))
        (cl-destructuring-bind (r g b a) (mapcar 'string-to-number rgb)
          (format "#%02X%02X%02X%02X" r g b (min 255 (round (* a 255))))))))

  (defun kurecolor-hex-to-rgba (hex)
    "Convert a 8 digit HEX color to r g b a."
    (setq hex (replace-regexp-in-string "#" "" hex))
    (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
            (list (substring hex 0 2)
                  (substring hex 2 4)
                  (substring hex 4 6)
                  (substring hex 6 8))))

  ;; Redefine this function to fully handle rgba
  (defun kurecolor-hex-to-cssrgb (hex)
    "Convert a HEX rgb color to cssrgb."
    (if (< (length hex) 8)
        (cl-destructuring-bind (r g b) (mapcar 'to-8bit (kurecolor-hex-to-rgb hex))
          (format "rgb(%i, %i, %i)" r g b))
      (cl-destructuring-bind (r g b a) (mapcar 'to-8bit (kurecolor-hex-to-rgba hex))
        (format "rgba(%i, %i, %i, %f)" r g b (/ a 255.0)))))

  (defun my-kurecolor-open-hydra ()
    "Makes sure hl-line-mode is off, color is in hex format, and opens the kurecolor hydra"
    (interactive)
    (hl-line-mode -1)
    (or (css--named-color-to-hex)
        (css--rgb-to-named-color-or-hex))
    (funcall #'+rgb/kurecolor-hydra/body))

  (map! :map (css-mode-map sass-mode-map stylus-mode-map)
        :localleader
        (:prefix ("c" . "colors")
         "c" #'css-cycle-color-format
         "k" #'my-kurecolor-open-hydra)))

(map! :leader
      (:prefix-map ("=" . "calc")
       "=" #'calc-dispatch
       "c" #'calc
       "q" #'quick-calc
       "g" #'calc-grab-region)
      (:prefix-map ("t" . "toggle")
       :desc "Git gutter" "v" #'git-gutter-mode
       :desc "Highlight line" "h" #'hl-line-mode))

(map! :map evil-window-map
      ;; Use the normal other-window command
      ;; to take advantage of the window-select module
      "w" #'other-window)

(map! :after dired
      :map dired-mode-map
      :n "RET" #'dired-find-alternate-file
      :desc "dired-up-director (alt)" :n "^" (lambda () (interactive) (find-alternate-file "..")))

(use-package! evil
  :config
  (setq evil-shift-width 2)
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  ;; (setq evil-escape-key-sequence "hl")
  ;; (setq evil-escape-unordered-key-sequence t)
  ;; (setq evil-escape-delay 0.05)
  (setq evil-snipe-scope 'visible)
  (defhydra my-mc-hydra (:color pink
                         :hint nil
                         :pre (evil-mc-pause-cursors))
    "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _z_: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   ^ ^                   _R_: remove all
_n_: skip & next   ^ ^                   _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
    ("Z" #'evil-mc-make-all-cursors)
    ("m" #'evil-mc-make-and-goto-next-match)
    ("M" #'evil-mc-make-and-goto-prev-match)
    ("n" #'evil-mc-skip-and-goto-next-match)
    ("N" #'evil-mc-skip-and-goto-prev-match)
    ("J" #'evil-mc-make-cursor-move-next-line)
    ("K" #'evil-mc-make-cursor-move-prev-line)
    ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
    ("r" #'+multiple-cursors/evil-mc-undo-cursor)
    ("R" #'evil-mc-undo-all-cursors)
    ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
    ("q" #'evil-mc-resume-cursors "quit" :color blue)
    ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))
  (defhydra my-sp-hydra (:color amaranth
                         :hint nil)
    "
^Navigation^           ^Editing^
^^^^-----------------  ---------------------------------------------------------
_l_: next sexp         _D_: kill hybrid      _T_: transpose hybrid
_h_: backward sexp     _d_: kill sexp        _t_: transpose
_b_: sexp beginning    _<_: barf backward    _r_: raise
_e_: sexp end          _>_: barf forward     _/_: split
_k_: up sexp (back)    _,_: slurp backward   _<delete>_: splice kill forward
_j_: down sexp         _._: slurp forward    _<backspace>_: splice kill backward
_J_: up sexp           _?_: slurp hybrid
_K_: down sexp (back)
_L: forward sexp
_H: previous sexp

_s_: toggle strict  _u_: undo  _C-r_: redo

"
    ("l" #'sp-next-sexp)
    ("h" #'sp-backward-sexp)
    ("L" #'sp-forward-sexp)
    ("H" #'sp-previous-sexp)
    ("b" #'sp-beginning-of-sexp)
    ("e" #'sp-end-of-sexp)
    ("k" #'sp-backward-up-sexp)
    ("j" #'sp-down-sexp)
    ("J" #'sp-up-sexp)
    ("K" #'sp-backward-down-sexp)
    ("D" #'sp-kill-hybrid-sexp)
    ("d" #'sp-kill-sexp)
    ("<" #'sp-backward-barf-sexp)
    (">" #'sp-forward-barf-sexp)
    ("," #'sp-backward-slurp-sexp)
    ("." #'sp-forward-slurp-sexp)
    ("?" #'sp-slurp-hybrid-sexp)
    ("t" #'sp-transpose-sexp)
    ("T" #'sp-transpose-hybrid-sexp)
    ("r" #'sp-raise-sexp)
    ("/" #'sp-split-sexp)
    ("|" #'sp-splice-sexp)
    ("<delete>" #'sp-splice-sexp-killing-forward)
    ("<backspace>" #'sp-splice-sexp-killing-backward)
    ("s" #'smartparens-strict-mode :exit t)
    ("u" #'undo-fu-only-undo)
    ("C-r" #'undo-fu-only-redo)
    ("q" nil "quit" :color blue)
    ("<escape>" nil "quit" :color blue))
  (map!
   (:when (featurep! :editor multiple-cursors)
    :prefix "g"
    :nv "z" #'my-mc-hydra/body)
   :prefix "gs"
   :nv "p" #'my-sp-hydra/body))

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
  ;; Advise `swiper-isearch' to use `rxt-quote-pcre' so that
  ;; SPC s s and SPC s S correctly deals with regex sensitive
  ;; characters in the selected region or symbol at point
  (defun my-rxt-quoted-swiper-isearch (orig-fun &rest args)
    (interactive)
    (apply orig-fun (mapcar #'rxt-quote-pcre args)))
  (advice-add #'swiper-isearch :around #'my-rxt-quoted-swiper-isearch))

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
    "--stdio")))

(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map)))

(use-package! ivy-rich
  :after ivy
  :config
  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
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
                   (lambda (x)
                     (ivy-rich-switch-buffer-shorten-path
                      x
                      (ivy-rich-minibuffer-width 0.3))))))
                :predicate
                (lambda (cand)
                  (get-buffer cand))))
  (ivy-rich-mode -1)
  (ivy-rich-mode +1))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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

;; Use 2-space indentation in css
(after! css-mode
  (setq css-indent-offset 2))

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
      ;; '(tab-line :inherit variable-pitch :foreground "black" :height 0.9)
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
      `(ediff-odd-diff-C     :background ,other-bg-C :extend t)))
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


;; Include ediff buffers in solaire-mode so they look the same
;; as regular editing buffers
(add-hook! 'ediff-prepare-buffer-hook #'solaire-mode)

;; Allow links to be opened outside WSL
(when (and (eq system-type 'gnu/linux)
           (string-match "Linux.*Microsoft.*Linux" (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

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

