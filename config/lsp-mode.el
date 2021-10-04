;;; config/lsp-mode.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq +lsp-defer-shutdown nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-log-io nil
        lsp-enable-folding nil
        lsp-semantic-tokens-enable nil
        lsp-enable-links nil
        lsp-enable-snippet nil
        lsp-signature-auto-activate nil
        lsp-auto-execute-action nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-actions-kind-regex "quickfix.*")

  ;; Don't autocomplete snippets in LSP by default.
  ;; It seems to interfere a lot more than it helps.
  ;; Use the C-x C-s binding to call company-snippets manually.
  (setq +lsp-company-backends '(:separate company-capf))

  ;; NOTE: Angular/TS language servers might perform better if we have this
  ;; (setenv "TSC_NONPOLLING_WATCHER" "1")
  (setq lsp-disabled-clients '(angular-ls)) ; disabled for performance for now. chews up so much CPU!
  ;; (setq lsp-clients-angular-language-server-command
  ;;       `("node"
  ;;         ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules/@angular/language-server")
  ;;         ;; "--experimental-ivy"
  ;;         "--ngProbeLocations"
  ;;         ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
  ;;         "--tsProbeLocations"
  ;;         ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
  ;;         "--stdio"))

  ;; Powershell support
  (use-package! powershell
    :hook (powershell-mode . lsp)
    :config
    (setq lsp-pwsh-exe "/usr/bin/pwsh")))

(after! lsp-ui
  ;; Fixes code action lightbulb icon background
  ;; It's probably a compatiblity issue with solaire mode
  (setq lsp-ui-sideline-actions-icon (doom-path doom-private-dir "assets" "lightbulb.png"))

  (custom-set-faces!
    `(lsp-ui-peek-highlight :foreground ,(doom-color 'yellow) :background ,(doom-color 'base4) :box t))

  ;; This fixes the TAB file toggle in lsp-ui peeking.
  ;; For some reason, evil's TAB keybinding takes precedence?
  ;; This rebinding is a hack to work around that.
  ;; Not sure of a better fix right now.
  (map! :map lsp-ui-peek-mode-map
        "<tab>" #'lsp-ui-peek--toggle-file))

;; must be set before package loads
(setq lsp-tailwindcss-add-on-mode t)
(use-package! lsp-tailwindcss
  :config
  (setq lsp-tailwindcss-major-modes '(rjsx-mode web-mode tsx-mode html-mode css-mode)))
