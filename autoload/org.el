;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it. Normally,
insert the headline’s title using the ’#title:’ file-level property and delete
the Org-mode headline. However, if the current headline has a Org-mode
properties drawer already, keep the headline and don’t insert ‘#+title:'.
Org-roam can extract the title from both kinds of notes, but using ‘#+title:’ is
a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (save-buffer)
    (org-roam-capture-
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file))
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

;;;###autoload
(defun my/doom-themes-enable-org-fontification-replacement (_orig-fun)
  (let ((org-todo (format org-heading-keyword-regexp-format
                          org-todo-regexp))
        (org-done (format org-heading-keyword-regexp-format
                          (concat "\\(?:" (mapconcat #'regexp-quote org-done-keywords
                                                     "\\|")
                                  "\\)"))))
    (setq
     org-font-lock-extra-keywords
     (append (org-delete-all
              (append `(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                         (0 (org-get-checkbox-statistics-face) t))
                        (,org-todo (2 (org-get-todo-face 2) t)))
                      (when org-fontify-done-headline
                        `((,org-done (2 'org-headline-done t))))
                      (when (memq 'date org-activate-links)
                        '((org-activate-dates (0 'org-date t)))))
              org-font-lock-extra-keywords)
             ;; respsect underlying faces!
             `((,org-todo (2 (org-get-todo-face 2) prepend)))
             (when org-fontify-done-headline
               `((,org-done (2 'org-headline-done prepend))))
             (when (memq 'date org-activate-links)
               '((org-activate-dates (0 'org-date prepend))))
             ;; Make checkbox statistic cookies respect underlying faces
             '(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                (0 (org-get-checkbox-statistics-face) prepend))
               ;; make plain list bullets stand out
               ("^ *\\([-+]\\|\\(?:[0-9]+\\|[a-zA-Z]\\)[).]\\)[ \t]" 1 'org-list-dt append)
               ;; and separators/dividers
               ("^ *\\(-----+\\)$" 1 'org-meta-line))
             ;; I like how org-mode fontifies checked TODOs and want this to
             ;; extend to checked checkbox items:
             (when org-fontify-done-headline
               '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                  1 'org-headline-done prepend)))
             ;; custom #hashtags & @at-tags for another level of organization
             (when doom-themes-org-fontify-special-tags
               '(("\\(?:\\s-\\|^\\)\\(\\([#@]\\)[A-Za-z0-9_.-:]+\\)" ; customized regex here to match times like 18:30
                  1 (doom-themes--org-tag-face 2) prepend)))))))
