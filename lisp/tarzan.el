;;; lisp/tarzan.el -*- lexical-binding: t; -*-

(defvar-local tarzan--working-nodes nil)

(defun tarzan--highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level."
  (save-excursion
    (goto-char position)
    (let* ((current-node (tree-sitter-node-at-pos))
           (parent-node (tsc-get-parent current-node)))
      (while (and parent-node
                  (not (eq 'program (tsc-node-type parent-node)))
                  (eq (tsc-node-start-byte parent-node)
                      (tsc-node-start-byte current-node)))
        (setq current-node parent-node)
        (setq parent-node (tsc-get-parent current-node)))
      current-node)))

(defun tarzan--goto-sibling (&optional backward)
  (or (> (if backward 0 (skip-chars-forward " \t\n\r")) 0)
      (let* ((node (tarzan--highest-node-at-position (point)))
             (target (if backward
                         (tsc-get-prev-sibling node)
                       (tsc-get-next-sibling node))))
        (if target
            (goto-char (tsc-node-start-position target))
          (goto-char (if backward
                         (tsc-node-start-position node)
                       (tsc-node-end-position node)))))))

(defun tarzan-goto-next-sibling ()
  (interactive)
  (tarzan--goto-sibling))

(defun tarzan-goto-next-sibling-end ()
  (interactive)
  (let ((current-pos (point)))
    (tarzan-goto-end)
    (when (eq (point) current-pos)
      (tarzan-goto-next-sibling)
      (tarzan-goto-end))))

(defun tarzan-goto-prev-sibling ()
  (interactive)
  (tarzan--goto-sibling t))

(defun tarzan-goto-prev-sibling-start ()
  (interactive)
  (let ((current-pos (point)))
    (tarzan-goto-start)
    (when (eq (point) current-pos)
      (tarzan-goto-prev-sibling)
      (tarzan-goto-start))))

(defun tarzan-goto-first-child ()
  (interactive)
  (let ((current-pos (point))
        (moved nil))
    (tsc-mapc-children
     (lambda (child)
       (let ((child-pos (tsc-node-start-position child)))
         (unless (or (eq child-pos (point))
                     moved)
           (goto-char child-pos)
           (setq moved t))))
     (tarzan--highest-node-at-position (point)))))

(defun tarzan-goto-parent ()
  (interactive)
  (let* ((node (tarzan--highest-node-at-position (point)))
         (target (tsc-get-parent node)))
    (goto-char (tsc-node-start-position target))))

(defun tarzan-goto-start ()
  (interactive)
  (let ((node (tree-sitter-node-at-pos)))
    (goto-char (tsc-node-start-position node))))

(defun tarzan-goto-end ()
  (interactive)
  (let ((node (tree-sitter-node-at-pos)))
    (goto-char (tarzan--evil-region-end-shim (tsc-node-end-position node)))))

(defun tarzan-expand-region ()
  (interactive)
  (unless (region-active-p)
    (setq tarzan--working-nodes nil))
  (when-let ((node (if tarzan--working-nodes
                       (tsc-get-parent (car tarzan--working-nodes))
                     (tree-sitter-node-at-pos))))
    (setq tarzan--working-nodes (cons node tarzan--working-nodes))
    (set-mark (tsc-node-start-position node))
    (goto-char (tarzan--evil-region-end-shim (tsc-node-end-position node)))
    (activate-mark)
    (message "Marked node: %s" (tsc-node-type node))))

(defun tarzan-contract-region ()
  (interactive)
  (when (and (region-active-p) tarzan--working-nodes)
    (when-let ((node (cadr tarzan--working-nodes)))
      (setq tarzan--working-nodes (cdr tarzan--working-nodes))
      (set-mark (tsc-node-start-position node))
      (goto-char (tarzan--evil-region-end-shim (tsc-node-end-position node)))
      (activate-mark)
      (message "Marked node: %s" (tsc-node-type node)))))

(defun tarzan--evil-region-end-shim (pos)
  (if (or (and (boundp 'evil-state) (eq evil-state 'operator))
          (and (fboundp #'evil-visual-state-p) (evil-visual-state-p)))
      pos (1- pos)))

(provide 'tarzan)
