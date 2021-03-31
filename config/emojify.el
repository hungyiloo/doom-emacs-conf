;;; config/emojify.el -*- lexical-binding: t; -*-

(use-package! emojify
  :commands (emojify-mode emojify-string)

  :config
  ;; I created a folder ~/.doom.d/.local/emojis/twemoji-latest and
  ;; downloaded the PNG assets from https://github.com/twitter/twemoji
  (setq emojify-download-emojis-p nil)
  (setq emojify-emoji-set "twemoji-latest")
  (setq emojify-emojis-dir (concat doom-private-dir ".local/emojis/"))
  ;; This file is generated via ~/.doom.d/generate-emoji-json.js
  ;; Run it using "node generate-emoji-json.js"
  (setq emojify-emoji-json (concat doom-private-dir "emoji.json"))

  (after! selectrum
    ;; Fix selectrum candidate emojification
    (advice-add #'selectrum--format-candidate :around #'my/emojify-result-advice))

  (after! consult
    ;; Fix emojify display issues after using consult.
    ;; `consult-line' and `consult-outline' in particular seems to mess with the buffer's
    ;; emojify region, so the following setup resets the global mode after use.
    (add-hook! 'consult-after-jump-hook #'emojify-redisplay-emojis-in-region)
    (advice-add #'consult-line :around #'my/emojify-reset-global-mode)
    (advice-add #'consult-outline :around #'my/emojify-reset-global-mode))

  ;; Override this function to fix "regexp too big" issues
  (advice-add #'emojify-set-emoji-data :around #'my/emojify-set-emoji-data-override)

  ;; Redefine this function to simplify the label on each emoji when inserting
  (advice-add #'emojify--get-completing-read-candidates :around #'my/emojify--get-completing-read-candidates-override)

  ;; Redefine this function to get rid of line spacing side effects in the
  ;; buffer when selection emojies
  (advice-add #'emojify-completing-read :around #'my/emojify-completing-read-override))
