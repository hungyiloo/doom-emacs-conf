;;; autoload/tsx-mode.el -*- lexical-binding: t; -*-

(defun tsx--mode-close-element ()
  (interactive)
  (when-let* ((nearest-container (or (save-excursion
                                       (backward-char 2)
                                       (tree-sitter-node-at-point 'jsx_element))
                                     (tree-sitter-node-at-point 'ERROR)))
              (opening-element (tsx--tsc-get-first-child-of-type nearest-container 'jsx_opening_element))
              (tag-name-node (tsx--tsc-get-first-child-of-type opening-element 'identifier))
              (tag-name (tsc-node-text tag-name-node))
              (closing-tag-markup (format "</%s>" tag-name)))
    (insert closing-tag-markup)
    t))

;;;###autoload
(defun tsx-mode-auto-close-element-maybe-h ()
  (interactive)
  (if (eq (char-before) ?<)
      (or (when (save-excursion (tsx--mode-close-element))
            (delete-char -1)
            (unless (eq (char-before) ?>)
              (search-forward ">" (line-end-position) t))
            (funcall indent-line-function))
          (insert "/"))
    (insert "/")))

(defun tsx--tsc-get-first-child-of-type (node type)
  (let* ((index 0)
         (child-node (tsc-get-nth-child node index)))
    (while (and child-node
                (not (eq (tsc-node-type child-node) type)))
      (setq index (1+ index))
      (setq child-node (tsc-get-nth-child node index)))
    (and child-node
         (eq (tsc-node-type child-node) type)
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

    ;; (message "%s container:%s%s node:%s%s"
    ;;          target-column
    ;;          (tsc-node-type container)
    ;;          (tsc-node-start-point container)
    ;;          (tsc-node-type node)
    ;;          (tsc-node-start-point node))

    (save-excursion (indent-line-to target-column))
    (skip-chars-forward " \t\n" (line-end-position))))
