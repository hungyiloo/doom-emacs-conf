;;; config/emojify.el -*- lexical-binding: t; -*-

(use-package! emojify
  :commands (emojify-mode emojify-string)
  :init
  (after! hydra
    (advice-add #'hydra-show-hint
                :around
                (defun my/emojify-hydra-hint (orig-fun hint caller)
                  (funcall
                   orig-fun
                   (cons (car hint)
                         (cons (emojify-string (cadr hint))
                               (cddr hint)))
                   caller))))
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
    (advice-add #'selectrum--format-candidate
                :around
                (defun my/emojify-selectrum-candidate (orig-fun &rest args)
                  (emojify-string (apply orig-fun args)))))

  (after! consult
    ;; Fix emojify display issues after using consult.
    ;; `consult-line' and `consult-outline' in particular seems to mess with the buffer's
    ;; emojify region, so the following setup resets the global mode after use.
    (defun my/emojify-reset-global-mode (orig-fun &rest args)
      (condition-case nil
          (progn
            (apply orig-fun args)
            (global-emojify-mode -1)
            (global-emojify-mode +1))
        (quit (progn (global-emojify-mode -1)
                     (global-emojify-mode +1)))))
    (add-hook! 'consult-after-jump-hook
               #'emojify-redisplay-emojis-in-region)
    (advice-add #'consult-line :around #'my/emojify-reset-global-mode)
    (advice-add #'consult-outline :around #'my/emojify-reset-global-mode))

  ;; Override this function to fix "regexp too big" issues
  (advice-add #'emojify-set-emoji-data :around #'my/emojify-set-emoji-data-override)

  ;; Redefine this function to simplify the label on each emoji when inserting
  (advice-add #'emojify--get-completing-read-candidates :around #'my/emojify--get-completing-read-candidates-override)

  ;; Redefine this function to get rid of line spacing side effects in the
  ;; buffer when selection emojies
  (advice-add #'emojify-completing-read :around #'my/emojify-completing-read-override))
