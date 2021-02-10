;;; config/projectile.el -*- lexical-binding: t; -*-

(after! projectile
  (setq projectile-kill-buffers-filter 'kill-all))
