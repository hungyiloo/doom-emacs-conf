;;; autoload/util.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/yank-rectangle-push-lines ()
  "Yank a rectangle as if it was an ordinary kill."
  (interactive "*")
  (if (use-region-p) (delete-region (region-beginning) (region-end)))
  (save-restriction
    (narrow-to-region (point) (point))
    (yank-rectangle)))
