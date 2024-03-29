;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hung-Yi Loo"
      user-mail-address "hungyi.loo@gmail.com")

;; I just keep coming back to Nord...
(setq doom-theme 'doom-nord)

;; Set a custom font. Font choice can be important for performance.
(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'light))

;; Better default window placement on startup
(setq initial-frame-alist '((width . 100) (height . 35) (fullscreen . fullheight)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/roam/journal/"
      org-roam-directory "~/Notes/roam/"
      org-agenda-files '("~/Notes/roam/journal/"))

;; Font adjustments should be more fine
(setq text-scale-mode-step 1.05)
(setq-default line-spacing 1)

;; Always revert files automatically
(global-auto-revert-mode 1)

;; Try out view-mode
(setq view-read-only t)

;; Better buffer names for files of the same name
;; NOTE: Setting this here messes up persp-mode *really* badly with duplicate
;; buffer names. This is especially painful when copying common files between
;; projects. Best just to live with the angle brackets...
;; (setq uniquify-buffer-name-style 'forward)

;; More emacs-y clipboard intergration
(setq save-interprogram-paste-before-kill t)

(setq calendar-date-style 'iso  ; ISO calendar by default
      calendar-week-start-day 1) ; weeks start on Mon; otherwise brain explodes

;; Re-enable some disabled commands
(put #'narrow-to-region 'disabled nil)

;; Always provide enough room for line numbers
(setq-default display-line-numbers-width-start t)

;; I prefer 2 space indenting
(setq standard-indent 2)
(setq-default tab-width 2)

;; Alt leader keys for WSLg workarounds
(setq doom-leader-alt-key "M-`")
(setq doom-localleader-alt-key "M-` m")

;; Some global keymap adjustments
(map! :leader
      ";" #'execute-extended-command
      ":" #'pp-eval-expression
      "]" #'better-jumper-jump-forward
      "[" #'better-jumper-jump-backward
      (:prefix-map ("=" . "calc")
                   "=" #'calc-dispatch
                   "l" #'literate-calc-minor-mode
                   "c" #'calc
                   "q" #'quick-calc
                   "g" #'calc-grab-region
                   "G" #'calc-grab-rectangle
                   (:prefix ("+" . "sum")
                            "+" #'calc-grab-sum-down
                            "=" #'calc-grab-sum-across))
      (:prefix-map ("t" . "toggle")
       :desc "Git gutter" "v" #'git-gutter-mode
       :desc "Highlight line" "h" #'hl-line-mode
       :desc "Debug on error" "d" #'toggle-debug-on-error)
      (:prefix-map ("p" . "project")
                   "v" #'projectile-run-vterm)
      (:prefix-map ("i" . "insert")
                   "u" #'insert-char
                   "e" #'emoji-insert-dwim
                   "E" #'emoji-insert
                   "i" #'nerd-icons-insert))

(map! :leader
      :desc "window hydra" "w" #'my/window-hydra/body
      :desc "window" "W" evil-window-map)

(map! :map evil-window-map
      ;; Use the normal other-window command
      ;; to take advantage of the window-select module
      "w" #'other-window
      "Q" #'kill-buffer-and-window
      "~" #'ace-swap-window)

(map! :map global-map
      "M-u" #'undo-only
      "M-[" #'previous-buffer
      "M-]" #'next-buffer
      "M-SPC" (cmd! (cycle-spacing -1))
      "<mouse-8>" #'better-jumper-jump-backward
      "<mouse-9>" #'better-jumper-jump-forward
      "<f12>" #'universal-argument
      :i "M-?" #'hippie-expand)

(map! :map minibuffer-mode-map
      "M-?" #'hippie-expand)

(map! :map universal-argument-map
      "<f12>" #'universal-argument-more)

(map! [remap dabbrev-expand] #'hippie-expand)

(setq +doom-dashboard-ascii-banner-fn
      (defun my/doom-dashboard-draw-ascii-banner-fn ()
        (let* ((banner
                '("░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                  "░░░░██╗░░██╗░██╗░░░██╗░░░░░░███████╗░███╗░░░███╗░░█████╗░░░█████╗░░░██████╗░░░░"
                  "░░░░██║░░██║░╚██╗░██╔╝░░░░░░██╔════╝░████╗░████║░██╔══██╗░██╔══██╗░██╔════╝░░░░"
                  "░░░░███████║░░╚████╔╝░░██╗░░█████╗░░░██╔████╔██║░███████║░██║░░╚═╝░╚█████╗░░░░░"
                  "░░░░██╔══██║░░░╚██╔╝░░░╚═╝░░██╔══╝░░░██║╚██╔╝██║░██╔══██║░██║░░██╗░░╚═══██╗░░░░"
                  "░░░░██║░░██║░░░░██║░░░░░░░░░███████╗░██║░╚═╝░██║░██║░░██║░╚█████╔╝░██████╔╝░░░░"
                  "░░░░╚═╝░░╚═╝░░░░╚═╝░░░░░░░░░╚══════╝░╚═╝░░░░░╚═╝░╚═╝░░╚═╝░░╚════╝░░╚═════╝░░░░░"
                  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                  "                                                                               "))
               (longest-line (apply #'max (mapcar #'length banner))))
          (put-text-property
           (point)
           (dolist (line banner (point))
             (insert (+doom-dashboard--center
                      +doom-dashboard--width
                      (concat
                       line (make-string (max 0 (- longest-line (length line)))
                                         32)))
                     "\n"))
           'face 'doom-dashboard-banner))))

(add-hook! 'doom-first-buffer-hook
  (setq display-line-numbers-type t))

;; This section is for UI and visual tweaks.
;; Not sure if there's a better place to put most of this stuff.
;; For now, this can be a catch all until I figure out how to defer them better.
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
         (treemacs-face-height 110)
         ;; (clearer-region (doom-blend (doom-color 'base4) (doom-color 'base3) 0.1))
         )
    (custom-set-faces!
      ;; Make tab bar background transparent so that it matches the theme
      ;; '(tab-line :inherit variable-pitch :foreground "black" :height 0.9)

      ;; More visible region background faces
      ;; `(region :background ,clearer-region)
      ;; `(evil-mc-region-face :background ,clearer-region)

      ;; Better styling for parentheses matching
      `(show-paren-match :background unspecified :foreground "#ff0000" :underline t)

      ;; Customize ediff highlighting
      `(ediff-fine-diff-A    :background ,current-bg-fine-A :weight unspecified)
      `(ediff-fine-diff-B    :background ,current-bg-fine-B :weight unspecified)
      `(ediff-fine-diff-C    :background ,current-bg-fine-C :weight unspecified)
      `(ediff-current-diff-A :background ,current-bg-A)
      `(ediff-current-diff-B :background ,current-bg-B)
      `(ediff-current-diff-C :background ,current-bg-C)
      `(ediff-even-diff-A    :background ,other-bg-A)
      `(ediff-even-diff-B    :background ,other-bg-B)
      `(ediff-even-diff-C    :background ,other-bg-C)
      `(ediff-odd-diff-A     :background ,other-bg-A)
      `(ediff-odd-diff-B     :background ,other-bg-B)
      `(ediff-odd-diff-C     :background ,other-bg-C)

      ;; Ignore ligatures in org-verbatim and org-code
      `(org-verbatim :family "JetBrains Mono NL")
      `(org-code     :family "JetBrains Mono NL")

      ;; Fix tree-sitter punctuation having stuck bg color
      `(tree-sitter-hl-face:punctuation)

      ;; Treemacs customizations
      `(treemacs-root-face :height 120)
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
  ;; Set decent default fonts for Japanese and Chinese,
  ;; but *only* if in a graphical context.
  ;; Set Japanese second so that Japanese glyphs override Chinese
  ;; when both charsets cover the same codepoints.
  (when (fboundp #'set-fontset-font)
    (set-fontset-font t 'chinese-gbk
                      ;; Noto Sans CJK: https://www.google.com/get/noto/help/cjk/
                      (font-spec :family "Noto Sans CJK SC" :weight 'regular))
    (set-fontset-font t 'japanese-jisx0213.2004-1
                      (font-spec :family "Noto Sans CJK JP" :weight 'regular))
    (set-fontset-font t 'thai
                      (font-spec :family "Noto Sans Thai" :weight 'regular)))
  (dolist (item '(("Noto Sans CJK JP" . 0.85)
                  ("Noto Sans CJK SC" . 0.85)))
    (add-to-list 'face-font-rescale-alist item))

  ;; Use as much of JetBrains Mono v2.221 as possible.
  ;; If a version >2.221 exists, see if new symbols are supported
  ;; and update below.
  (set-fontset-font t ?» (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?» (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?« (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?“ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?” (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?λ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?ƒ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∅ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⊤ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?⊥ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?ℤ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?ℝ (font-spec :family "Fira Code"))
  (set-fontset-font t ?𝔹 (font-spec :family "Fira Code"))
  (set-fontset-font t ?ℂ (font-spec :family "Fira Code"))
  (set-fontset-font t ?∈ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∉ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∧ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∨ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∀ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∃ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?⟼ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⟻ (font-spec :family "Fira Code"))
  (set-fontset-font t ?∪ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∩ (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?∖ (font-spec :family "Free Mono"))
  (set-fontset-font t ?⨂ (font-spec :family "Free Mono"))
  (set-fontset-font t ?• (font-spec :family "JetBrains Mono"))
  (set-fontset-font t ?⅓ (font-spec :family "Fira Code"))
  (set-fontset-font t ?⅔ (font-spec :family "Fira Code"))

  ;; Don't accelerate mouse wheel scrolling
  (setq mouse-wheel-scroll-amount '(3
                                    ((shift)
                                     . hscroll)
                                    ((meta))
                                    ((control)
                                     . text-scale)))
  (setq mouse-wheel-progressive-speed nil)
  ;; Animate mouse wheel scrolling
  (setq pixel-scroll-precision-large-scroll-height 40.0)

  ;; Tweak help popups
  (set-popup-rule!
    "^\\*helpful function"
    :side 'bottom :height 20 :width 40 :quit t :select t :ttl 5)

  ;; Pixel scrolling
  (when (fboundp #'pixel-scroll-precision-mode) (pixel-scroll-precision-mode +1)))


;; Load some external files
(dolist (dir (list "config" ".local"))
  (dolist (file (directory-files
                 (doom-path doom-private-dir dir)
                 t
                 ".+\\.el$"))
    (load! file)))

(setq safe-local-variable-values '((org-agenda-files . ("~/work/notes/"))
                                   (org-directory . "~/code/hungyi.net/")
                                   (lsp-disabled-clients . (ts-ls))
                                   (lsp-disabled-clients . nil)))
