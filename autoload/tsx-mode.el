;;; autoload/tsx-mode.el -*- lexical-binding: t; -*-

(defun tsx--element-close ()
  (when-let* ((nearest-container (or (save-excursion
                                       (backward-char 2)
                                       (tsx--element-at-point))
                                     (tree-sitter-node-at-point 'ERROR)))
              (tag-name (tsx--element-tag-name nearest-container))
              (closing-tag-markup (format "</%s>" tag-name)))
    (insert closing-tag-markup)
    t))

;;;###autoload
(defun tsx-element-auto-close-maybe-h ()
  (interactive)
  (if (eq (char-before) ?<)
      (or (when (save-excursion (tsx--element-close))
            (delete-char -1)
            (unless (eq (char-before) ?>)
              (search-forward ">" (line-end-position) t))
            (funcall indent-line-function))
          (insert "/"))
    (insert "/")))

(defun tsx--element-tag-nodes (element-node)
  (when-let* ((opening-element (tsx--tsc-first-child-of-type
                                element-node
                                '(jsx_opening_element)))
              (closing-element (tsx--tsc-first-child-of-type
                                element-node
                                '(jsx_closing_element))))
    `(,(tsx--tsc-first-child-of-type
        opening-element
        '(identifier nested_identifier))
      .
      ,(tsx--tsc-first-child-of-type
        closing-element
        '(identifier nested_identifier)))))

(defun tsx--element-tag-name (element-node)
  (when-let* ((opening-element (tsx--tsc-first-child-of-type
                                element-node
                                '(jsx_opening_element)))
              (tag-name-node (car (tsx--element-tag-nodes element-node))))
    (tsc-node-text tag-name-node)))

(defun tsx--element-at-point ()
  (tree-sitter-node-at-point 'jsx_element))

(defun tsx--replace-node (node replacement-text)
  (replace-region-contents
   (tsc-node-start-position node)
   (tsc-node-end-position node)
   (lambda () replacement-text)))

;;;###autoload
(defun tsx-element-rename ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point))
              (tag-nodes (tsx--element-tag-nodes element-node))
              (tag-name (tsx--element-tag-name element-node))
              (new-tag-name (string-trim (read-string "Rename element: " tag-name))))
    (unless (> (length new-tag-name) 0) (user-error "Oops! That isn't a valid tag name..."))
    (tsx--replace-node (cdr tag-nodes) new-tag-name)
    (tsx--replace-node (car tag-nodes) new-tag-name)))

(defun tsx--tsc-first-child-of-type (node types)
  (let* ((index 0)
         (child-node (tsc-get-nth-child node index)))
    (while (and child-node
                (not (memq (tsc-node-type child-node) types)))
      (setq index (1+ index))
      (setq child-node (tsc-get-nth-child node index)))
    (and child-node
         (memq (tsc-node-type child-node) types)
         child-node)))

(defun tsx--tsc-highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\)."
  (save-excursion
    (goto-char position)
    (let* ((current-node (tree-sitter-node-at-point))
           (parent-node (tsc-get-parent current-node)))
      (while (and parent-node
                  (eq (tsc-node-start-byte parent-node)
                      (tsc-node-start-byte current-node)))
        (setq current-node parent-node)
        (setq parent-node (tsc-get-parent current-node)))
      current-node)))

;;;###autoload
(defun tsx-indent-line-function ()
  (let* ((curr-point (save-excursion (back-to-indentation) (point)))
         (curr-column (current-indentation))
         (node (tsx--tsc-highest-node-at-position curr-point))
         (node-line (car (tsc-node-start-point node)))
         (node-type (tsc-node-type node))
         (curr-line (line-number-at-pos curr-point))
         (container (if (or (memq node-type '(jsx_text))
                            (= curr-line node-line))
                        (or (tsc-get-parent node) node)
                      node))
         (container-column (save-excursion
                             (goto-char (car (tsc-node-position-range container)))
                             (current-indentation)))
         (target-column (cond
                         ((memq (tsc-node-type container) '(string template_string program)) 0)
                         ((memq (tsc-node-type node) '(string template_string)) 0)
                         ((member node-type '("[" "(" "{" "}" ")" "]" "<" "/"
                                              jsx_closing_element
                                              statement_block
                                              else_clause))
                          (if (and (> curr-line node-line)
                                   (< curr-line (car (tsc-node-end-point node))))
                              (+ container-column js-indent-level)
                            container-column))
                         ((> curr-point (car (tsc-node-position-range container)))
                          (+ container-column js-indent-level))
                         (t curr-column))))
    (save-excursion (indent-line-to target-column))
    (skip-chars-forward " \t\n" (line-end-position))))
