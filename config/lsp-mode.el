;;; config/lsp-mode.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq +lsp-defer-shutdown nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-log-io nil
        lsp-enable-folding nil
        lsp-semantic-tokens-enable nil
        lsp-enable-links nil
        lsp-enable-snippet t)

  ;; Don't autocomplete snippets in LSP by default.
  ;; It seems to interfere a lot more than it helps.
  ;; Use the C-x C-s binding to call company-snippets manually.
  (setq +lsp-company-backends '(:separate company-capf))


  (setq lsp-clients-typescript-server-args
        `("--stdio"
          ,(concat "--tsserver-path=" (getenv "HOME") "/.yarn/bin/tsserver")))

  ;; NOTE: Angular/TS language servers might perform better if we have this
  ;; (setenv "TSC_NONPOLLING_WATCHER" "1")
  (setq lsp-disabled-clients '(angular-ls)) ; disabled for performance for now. chews up so much CPU!
  (setq lsp-clients-angular-language-server-command
        `("node"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules/@angular/language-server")
          ;; "--experimental-ivy"
          "--ngProbeLocations"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
          "--tsProbeLocations"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
          "--stdio"))

  ;; Powershell support
  (use-package! powershell
    :hook (powershell-mode . lsp)
    :config
    (setq lsp-pwsh-exe "/usr/bin/pwsh"))

  ;; Load the lsp-command-map earlier, up front.
  ;; In practice, it's never there early enough when I need it.
  ;; e.g. without this, I keep getting "SPC c l s r is undefined"
  ;; when entering keychords quickly.
  ;; With this, it seems to be loaded along with lsp-mode
  (map! :leader "c l" lsp-command-map)
  (dolist (leader-key (list doom-leader-key doom-leader-alt-key))
    (let ((lsp-keymap-prefix (concat leader-key " c l")))
      (lsp-enable-which-key-integration))))

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
