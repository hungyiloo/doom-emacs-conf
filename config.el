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
(setq doom-theme 'doom-nord)

;; Set a custom font
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Segoe UI"))

(setq initial-frame-alist '((width . 141) (height . 55) (fullscreen . fullheight)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

(after! org
  ;; Log CLOSED timestamp when notes are set to DONE state
  (setq org-log-done 'time)
  ;; Show more whitespace in org mode when cycling
  (setq org-cycle-separator-lines -1)
  ;; Always use ID properties to store links
  (setq org-id-link-to-org-use-id 'use-existing)
  ;; Set custom header bullets
  (setq org-superstar-headline-bullets-list '("★" "▶" "◾" "•" "·"))
  (setq org-superstar-cycle-headline-bullets nil)
  ;; Always use fast plain lists in org-superstar
  (org-superstar-toggle-lightweight-lists))

(after! evil
  ;; Don't move backwards
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t))

(after! centaur-tabs
  (setq centaur-tabs-modified-marker "■")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-bar nil)
  (setq centaur-tabs-style "wave"))

(after! avy
  ;; Configure avy to use colemak home row
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(after! neotree
  ;; Allow resizing of neotree window
  (setq neo-window-fixed-size nil)
  ;; Don't reset neotree window size when opening a file
  (add-to-list 'window-size-change-functions
               (lambda (frame)
                 (let ((neo-window (neo-global--get-window)))
                   (unless (null neo-window)
                     (setq neo-window-width (window-width neo-window))))))
  ;; Keep neotree window size when opening/closing
  (defun neo-window--zoom (method)
    "Zoom the NeoTree window, the METHOD should one of these options:
     'maximize 'minimize 'zoom-in 'zoom-out."
    (neo-buffer--unlock-width)
    (cond
     ((eq method 'maximize)
      (maximize-window))
     ((eq method 'minimize)
      (message "neotree override: its window width will not be reset"))
     ((eq method 'zoom-in)
      (shrink-window-horizontally 2))
     ((eq method 'zoom-out)
      (enlarge-window-horizontally 2)))
    (neo-buffer--lock-width))
  ;; Enable richer icons on neotree
  (setq doom-themes-neotree-enable-file-icons t))

;; When storing links by ID, add them to the normal `org-stored-links' variable
(defadvice! +org--store-id-link-a (link)
  :filter-return #'org-id-store-link
  (when (and link org-store-link-plist)
    (add-to-list 'org-stored-links
                 (list (plist-get org-store-link-plist :link)
                       (plist-get org-store-link-plist :description))))
  link)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-line ((t (:inherit variable-pitch :foreground "black" :height 0.9)))))

(use-package! composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                   (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                   (36 . ".\\(?:>\\)")
                   (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                   (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                   (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                   (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                   (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                   (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                   (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                   (48 . ".\\(?:x[a-zA-Z]\\)")
                   (58 . ".\\(?:::\\|[:=]\\)")
                   (59 . ".\\(?:;;\\|;\\)")
                   (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                   (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                   (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                   (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                   (91 . ".\\(?:]\\)")
                   (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                   (94 . ".\\(?:=\\)")
                   (119 . ".\\(?:ww\\)")
                   (123 . ".\\(?:-\\)")
                   (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                   (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                   )
                 ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))
  )
