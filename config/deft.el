;;; config/deft.el -*- lexical-binding: t; -*-

(after! deft
  (require 'org)
  (setq deft-directory org-roam-directory))
