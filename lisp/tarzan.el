;;; lisp/tarzan.el -*- lexical-binding: t; -*-

(defvar-local tarzan--working-nodes nil)
(defvar-local tarzan--working-node nil)
(defvar-local tarzan--working-node-overlay nil)

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

(defun tarzan--get-node ()
  (unless (and tarzan--working-node
               (<= (tsc-node-start-position tarzan--working-node)
                   (point)
                   (tsc-node-end-position tarzan--working-node)))
    (tarzan--set-working-node (tree-sitter-node-at-pos)))
  tarzan--working-node)

(defun tarzan--set-working-node (node)
  (setq tarzan--working-node node)
  (let ((beg (tsc-node-start-position node))
        (end (tsc-node-end-position node)))
    (if tarzan--working-node-overlay
        (move-overlay tarzan--working-node-overlay beg end)
      (progn
        (setq tarzan--working-node-overlay (make-overlay beg end (current-buffer)))
        (overlay-put tarzan--working-node-overlay 'face '(:background "black"))))))

(defun tarzan--goto-sibling (&optional backward)
  (when-let* ((node (tarzan--get-node))
              (target (if backward
                          (tsc-get-prev-sibling node)
                        (tsc-get-next-sibling node))))
    (goto-char (tsc-node-start-position target))
    (tarzan--set-working-node target)))

(defun tarzan-goto-next-sibling ()
  (interactive)
  (tarzan--goto-sibling))

(defun tarzan-goto-next-sibling-start ()
  (interactive)
  (tarzan-goto-next-sibling)
  (tarzan-goto-start))

(defun tarzan-goto-prev-sibling-end ()
  (interactive)
  (tarzan-goto-prev-sibling)
  (tarzan-goto-end))

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
  (when-let* ((node (tarzan--get-node))
              (child (tsc-get-nth-child node 0)))
    (goto-char (tsc-node-start-position child))
    (tarzan--set-working-node child)))

(defun tarzan-goto-parent ()
  (interactive)
  (when-let* ((node (tarzan--get-node))
              (target (tsc-get-parent node)))
    (goto-char (tsc-node-start-position target))
    (tarzan--set-working-node target)))

(defun tarzan-goto-start ()
  (interactive)
  (let ((node (tarzan--get-node)))
    (goto-char (tsc-node-start-position node))))

(defun tarzan-goto-end ()
  (interactive)
  (let ((node (tarzan--get-node)))
    (goto-char (tarzan--evil-region-end-shim (tsc-node-end-position node)))))

(defun tarzan-expand-region ()
  (interactive)
  (unless (region-active-p)
    (setq tarzan--working-nodes nil))
  (when-let ((node (if tarzan--working-nodes
                       (tsc-get-parent (car tarzan--working-nodes))
                     (tarzan--get-node))))
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
