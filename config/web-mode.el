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
  ;; and prepopulate minibuffer with existing tag name
  (defun web-mode-element-rename (&optional tag-name)
    "Rename the current html element."
    (interactive)
    (save-excursion
      (when (and (web-mode-element-beginning)
                 ;; Changed this from ? to * ------------------------v
                 (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)"))
        (let* ((pos (point))
               (previous-tag-name (buffer-substring-no-properties (1+ (match-beginning 0)) (match-end 0)))
               (tag-name (or tag-name
                             (read-from-minibuffer "New tag name? " previous-tag-name))))
          (when (and (> (length tag-name) 0)
                     (not (web-mode-element-is-void)))
            (save-match-data
              (web-mode-tag-match)
              ;; Changed this from ? to * -----------------------------------v
              (when (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)*\\)")
                (replace-match (concat "</" tag-name))))
            (goto-char pos)
            (replace-match (concat "<" tag-name)))))))

  (defun my/web-mode-element-split-to-lines ()
    "Splits the element into multiple lines by HTML tags in a generally beautified, sparse style."
    (interactive)
    (save-excursion
      (web-mode-element-beginning)
      (when (search-backward-regexp "[^ ]" (line-beginning-position) t)
        (web-mode-element-next)
        (newline-and-indent))
      (let ((separator "\\(>[[:blank:]]*?[^\\n]\\|[^\\n][[:blank:]]*?<\\)")
            (beg-pos (web-mode-element-beginning-position))
            (end-pos (web-mode-element-end-position)))
        (while (<= (point) end-pos)
          (if (search-forward-regexp separator (min end-pos (line-end-position)) t)
              (progn
                (backward-char)
                (newline-and-indent)
                (save-excursion
                  (goto-char beg-pos)
                  (setq end-pos (web-mode-element-end-position)))
                (forward-char))
            (web-mode-tag-end)
            (web-mode-tag-next)))
        (goto-char (1+ end-pos))
        (unless (eq ?\n (char-after))
          (newline-and-indent)))))

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
        (setq web-mode-enable-auto-indentation t))))

  (map! :map web-mode-map
        :nv "SPC m e `" #'web-mode-navigate
        :nv "SPC m e RET" #'my/web-mode-element-split-to-lines))
