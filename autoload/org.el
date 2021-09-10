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
