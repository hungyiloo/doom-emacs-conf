;;; config/web-mode.el -*- lexical-binding: t; -*-

(after! web-mode
  (setq web-mode-prettify-symbols-alist nil)
  ;; Use 2-space indentation in web-mode always
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset nil)
  (setq emmet-indentation 2)
  (setq web-mode-enable-css-colorization nil)

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

  ;; Redefine this function to better handle when whole lines are selected
  (defun web-mode-element-wrap (&optional tag-name)
    "Wrap current REGION with start and end tags.
Prompt user if TAG-NAME isn't provided."
    (interactive)
    (let (beg end pos tag sep)
      (save-excursion
        (setq tag (or tag-name (web-mode-element-complete)))
        (setq pos (point))
        (cond
         (mark-active
          (setq beg (region-beginning)
                end (region-end))
          ;; These next two sexps adjust the region beg/end
          ;; to go to the first and last non-whitespace chars
          ;; of the selected region, but only if the previously
          ;; selected beg/end positions were at the beginning-of-line,
          ;; which is how the evil visual line selection works
          (save-excursion
            (goto-char beg)
            (when (bolp)
              (back-to-indentation)
              (setq beg (point))))
          (save-excursion
            (goto-char end)
            (when (bolp)
              (backward-char)
              (skip-syntax-backward " " (pos-bol))
              (setq end (point)))))
         ((get-text-property pos 'tag-type)
          (setq beg (web-mode-element-beginning-position pos)
                end (1+ (web-mode-element-end-position pos))))
         ((setq beg (web-mode-element-parent-position pos))
          (setq end (1+ (web-mode-element-end-position pos))))
         )
        ;;      (message "beg(%S) end(%S)" beg end)
        (when (and beg end (> end 0))
          (setq sep (if (get-text-property beg 'tag-beg) "\n" ""))
          (web-mode-insert-text-at-pos (concat sep "</" tag ">") end)
          (web-mode-insert-text-at-pos (concat "<" tag ">" sep) beg)
          (when (string= sep "\n") (indent-region beg (+ end (* (+ 3 (length tag)) 2))))
          )
        ) ;save-excursion
      (web-mode-go beg)))

  (defun my/web-mode-element-split-to-lines ()
    "Splits the element into multiple lines by HTML tags in a generally beautified, sparse style."
    (interactive)
    (save-excursion
      (web-mode-element-beginning)
      (let ((beg-pos (web-mode-element-beginning-position))
            (end-pos (web-mode-element-end-position)))
        (while (< (point) end-pos)
          (when (save-excursion (search-backward-regexp "[^ ]" (line-beginning-position) t))
            (newline-and-indent))
          (web-mode-tag-end)
          (when (eq (point)
                    (save-excursion (backward-char) (web-mode-tag-match-position)))
            (web-mode-tag-end))
          (progn
            (when (save-excursion (search-forward-regexp "[^ ]" (line-end-position) t))
              (newline-and-indent))
            (when (not (eq (char-after) ?<))
              (web-mode-tag-next)))
          (save-excursion
            (goto-char beg-pos)
            (setq end-pos (web-mode-element-end-position))))
        (indent-region beg-pos end-pos))))
  (defun my/web-mode-tag-split-to-lines ()
    "Splits the tag into multiple lines by attributes."
    (interactive)
    (save-excursion
      (web-mode-tag-beginning)
      (let ((beg-pos (web-mode-tag-beginning-position))
            (end-pos (web-mode-tag-end-position)))
        (web-mode-attribute-next)
        (web-mode-attribute-next)
        (while (< (point) end-pos)
          (when (save-excursion (search-backward-regexp "[^ ]" (line-beginning-position) t))
            (newline-and-indent)
            (setq end-pos (web-mode-tag-end-position)))
          (or (web-mode-attribute-next)
              (goto-char (1+ end-pos)))))))

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
        :nv "SPC m e RET" #'my/web-mode-element-split-to-lines
        :nv "SPC m t RET" #'my/web-mode-tag-split-to-lines))


;; use web-mode for php
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
