;;; config/web-mode.el -*- lexical-binding: t; -*-

(after! web-mode
  (setq web-mode-prettify-symbols-alist nil)
  ;; Use 2-space indentation in web-mode always
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset nil)
  (setq emmet-indentation 2)
  ;; Redefine this function to fix incomplete tag renames
  (defun web-mode-element-rename (&optional tag-name)
    "Rename the current html element."
    (interactive)
    (save-excursion
      (let (pos)
        (unless tag-name (setq tag-name (read-from-minibuffer "New tag name? ")))
        (when (and (> (length tag-name) 0)
                   (web-mode-element-beginning)
                   ;; Changed this from ? to * ------------------------v
                   (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)"))
          (setq pos (point))
          (unless (web-mode-element-is-void)
            (save-match-data
              (web-mode-tag-match)
              ;; Changed this from ? to * ---------------------------------v
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)")
                  (replace-match (concat "</" tag-name))
                )))
          (goto-char pos)
          (replace-match (concat "<" tag-name))
          ))))

  ;; evil-mc doesn't deal well with "auto" web-mode behaviors so temporarily
  ;; disable them while we have multiple cursors in flight.
  (after! evil-mc
    (add-hook! 'evil-mc-before-cursors-created
      (when (eq major-mode 'web-mode)
        (setq web-mode-enable-auto-closing nil)
        (setq web-mode-enable-auto-opening nil)
        (setq web-mode-enable-auto-pairing nil)
        (setq web-mode-enable-auto-indentation nil)))
    (add-hook! 'evil-mc-after-cursors-deleted
      (when (eq major-mode 'web-mode)
        (setq web-mode-enable-auto-closing t)
        (setq web-mode-enable-auto-opening t)
        (setq web-mode-enable-auto-pairing t)
        (setq web-mode-enable-auto-indentation t)))))
