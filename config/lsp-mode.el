;;; config/lsp-mode.el -*- lexical-binding: t; -*-

(after! lsp-mode
  ;; Setting this disables DOOM's deferred shutdown functionality.
  (setq +lsp-defer-shutdown nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-file-watchers nil)

  (setq lsp-clients-typescript-server-args
        `("--stdio"
          ,(concat "--tsserver-path=" (getenv "HOME") "/.yarn/bin/tsserver")))

  ;; NOTE: Angular/TS language servers might perform better if we have this
  ;; (setenv "TSC_NONPOLLING_WATCHER" "1")
  (setq lsp-disabled-clients '(angular-ls)) ; disabled for performance for now. chews up so much CPU!
  (setq lsp-clients-angular-language-server-command
        `("node"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules/@angular/language-server")
          "--ngProbeLocations"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
          "--tsProbeLocations"
          ,(doom-path (getenv "HOME") ".config/yarn/global/node_modules")
          "--stdio"))

  ;; (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-semantic-tokens-enable t))
