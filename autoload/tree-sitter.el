;;; autoload/tree-sitter.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-tree-sitter-point-in-comment-p (&optional pos)
    "Use tree-sitter to determine if point is inside comment"
    (let ((pos (or pos (point))))
      (and (not (= (point-min) pos))
           (save-excursion
             (goto-char (1- pos))
             (tree-sitter-node-at-point 'comment nil t)))))
