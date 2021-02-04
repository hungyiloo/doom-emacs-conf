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
          "--tsserver-path=/home/hungyi/.yarn/bin/tsserver"))
  ;; (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-semantic-tokens-enable t))
