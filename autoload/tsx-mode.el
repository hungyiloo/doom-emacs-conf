;;; autoload/tsx-mode.el -*- lexical-binding: t; -*-

(require 'tree-sitter)
(require 'seq)
(require 'cl-lib)

;;;###autoload
(defun tsx-element-close (&optional dont-indent)
  (interactive)
  (when-let* ((nearest-container (tsx--closest-parent-node (1- (point)) '(jsx_element ERROR)))
              (tag-name (tsx--element-tag-name nearest-container))
              (closing-tag-markup (format "</%s>" tag-name)))
    (insert closing-tag-markup)
    (unless dont-indent
      (funcall indent-line-function))
    t))

;;;###autoload
(defun tsx-element-auto-close-maybe-h ()
  (interactive)
  (if (eq (char-before) ?<)
      (let* ((before-tag-pos (- (point) 1))
             (empty-tag (eq (char-before before-tag-pos) ?>)))
        (or (when (save-excursion (backward-char) (tsx-element-close t))
              (delete-char -1)
              (when empty-tag (goto-char before-tag-pos))
              (funcall indent-line-function))
            (insert "/")))
    (insert "/")))

(defun tsx--element-tag-nodes (element-node)
  (let ((opening-element (tsx--tsc-first-child-of-type
                          element-node
                          '(jsx_opening_element)))
        (closing-element (tsx--tsc-first-child-of-type
                          element-node
                          '(jsx_closing_element)))
        (self-closing-element (and (eq 'jsx_self_closing_element
                                       (tsc-node-type element-node))
                                   element-node)))
    (seq-filter
     (lambda (n) n)
     (list
      closing-element
      opening-element
      self-closing-element))))

(defun tsx--element-tag-name-nodes (element-node)
  (let ((tag-nodes (tsx--element-tag-nodes element-node)))
    (seq-filter
     (lambda (n) n)
     (mapcar
      (lambda (n)
        (tsx--tsc-first-child-of-type n '(identifier nested_identifier)))
      tag-nodes))))

(defun tsx--element-tag-name (element-node)
  (when-let* ((tag-name-node (car (tsx--element-tag-name-nodes element-node))))
    (tsc-node-text tag-name-node)))

(defun tsx--closest-parent-node (&optional pos node-types)
  "Gets the closest node at POS (point if nil) that matches one of NODE-TYPES
(smallest node if nil)"
  (let* ((root (tsc-root-node tree-sitter-tree))
         (pos (or pos (point)))
         (node (tsc-get-descendant-for-position-range root pos pos)))
    (if node-types
        (let ((this node) result)
          (while this
            (if (memq (tsc-node-type this) node-types)
                (setq result this
                      this nil)
              (setq this (tsc-get-parent this))))
          result)
      node)))

(defun tsx--element-at-point (&optional include-self-closing)
  (tsx--closest-parent-node nil (list 'jsx_element
                               (when include-self-closing 'jsx_self_closing_element))))

(defun tsx--tag-at-point ()
  (tsx--closest-parent-node nil '(jsx_opening_element jsx_closing_element jsx_self_closing_element)))

(defun tsx--attribute-at-point ()
  (tsx--closest-parent-node nil '(jsx_attribute)))

(defun tsx--replace-node (node replacement-text)
  (replace-region-contents
   (tsc-node-start-position node)
   (tsc-node-end-position node)
   (lambda () replacement-text)))

;;;###autoload
(defun tsx-element-rename ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point t))
              (tag-nodes (tsx--element-tag-name-nodes element-node))
              (first-tag-node (car tag-nodes))
              (tag-name (tsc-node-text first-tag-node))
              (new-tag-name (string-trim (read-string "Rename element: " tag-name))))
    (unless (> (length new-tag-name) 0) (user-error "Oops! That isn't a valid tag name..."))
    (evil-with-single-undo
      (dolist (tag-node tag-nodes)
        (tsx--replace-node tag-node new-tag-name)))))

;;;###autoload
(defun tsx-element-wrap ()
  (interactive)
  (let* ((wrap-region
          (or (and (region-active-p)
                   (cons (region-beginning) (region-end)))
              (when-let ((node (tsx--element-at-point t)))
                (cons (tsc-node-start-position node) (tsc-node-end-position node)))
              (bounds-of-thing-at-point 'symbol)))
         (wrap-tag-name (string-trim (read-string "Wrapping element: ")))
         (wrap-start (car wrap-region))
         (wrap-end (cdr wrap-region))
         (region-linewise (and (region-active-p)
                               (= 0 (save-excursion
                                      (goto-char wrap-start)
                                      (current-column)))))
         (element-linewise (not (or (region-active-p)
                                    (eq (line-number-at-pos wrap-start)
                                        (line-number-at-pos wrap-end)))))
         (replacement (concat (format "<%s>" wrap-tag-name)
                              (when (or element-linewise region-linewise) "\n")
                              (buffer-substring wrap-start wrap-end)
                              (when element-linewise "\n")
                              (format "</%s>" wrap-tag-name)
                              (when region-linewise "\n"))))
    (unless (> (length wrap-tag-name) 0) (user-error "Oops! That isn't a valid tag name..."))
    (replace-region-contents
     wrap-start wrap-end
     (lambda () replacement))
    (indent-region wrap-start (+ wrap-start (length replacement)))))

(defun tsx--node-select (node)
  (when-let* ((node-start (tsc-node-start-position node))
              (node-end (tsx--evil-region-end-shim (tsc-node-end-position node))))
    (set-mark node-start)
    (goto-char node-end)
    (activate-mark)))

(defun tsx--node-delete (node &optional push-kill clean-whitespace)
  (when-let* ((node-start (tsc-node-start-position node))
              (node-end (tsc-node-end-position node))
              (node-start (if (or (eq clean-whitespace t)
                                  (eq clean-whitespace 'before))
                              (save-excursion
                                (goto-char node-start)
                                (skip-chars-backward " \t\n\r")
                                (point))
                            node-start))
              (node-end (if (or (eq clean-whitespace t)
                                (eq clean-whitespace 'after))
                            (save-excursion
                              (goto-char node-end)
                              (skip-chars-forward " \t\n\r")
                              (point))
                          node-end)))
    (funcall (if push-kill #'kill-region #'delete-region)
             node-start node-end)))

;;;###autoload
(defun tsx--evil-region-end-shim (pos)
  (if (or (and (boundp 'evil-state) (eq evil-state 'operator))
          (and (fboundp #'evil-visual-state-p) (evil-visual-state-p)))
      pos (1- pos)))

;;;###autoload
(defun tsx-element-select ()
  (interactive)
  (when-let ((node (tsx--element-at-point t)))
    (tsx--node-select node)))

;;;###autoload
(defun tsx-element-kill ()
  (interactive)
  (when-let ((node (tsx--element-at-point t)))
    (tsx--node-delete node t 'after)))

;;;###autoload
(defun tsx-tag-select ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-select node)))

;;;###autoload
(defun tsx-tag-kill ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-delete node t 'after)))

;;;###autoload
(defun tsx-attribute-select ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (tsx--node-select node)))

;;;###autoload
(defun tsx-attribute-kill ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (tsx--node-delete node t 'after)))

;;;###autoload
(defun tsx-element-select-content ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point))
              (opening-node (tsx--tsc-first-child-of-type element-node '(jsx_opening_element)))
              (closing-node (tsx--tsc-first-child-of-type element-node '(jsx_closing_element)))
              (beg (tsc-node-end-position opening-node))
              (end (tsx--evil-region-end-shim (tsc-node-start-position closing-node))))
    (when (< end beg) (user-error "Element is empty; nothing to select"))
    (set-mark beg)
    (goto-char end)
    (activate-mark)))

;;;###autoload
(defun tsx-goto-element-end ()
  (interactive)
  (when-let* ((node (tsx--element-at-point t))
              (closing-element (or (and (eq (tsc-node-type node) 'jsx_self_closing_element) node)
                                   (tsx--tsc-first-child-of-type node '(jsx_closing_element))))
              (target-pos (tsc-node-end-position closing-element)))
    (goto-char target-pos)))

;;;###autoload
(defun tsx-goto-element-beginning ()
  (interactive)
  (when-let* ((node (save-excursion
                      (backward-char)
                      (tsx--element-at-point t)))
              (opening-element (or (and (eq (tsc-node-type node) 'jsx_self_closing_element) node)
                                   (tsx--tsc-first-child-of-type node '(jsx_opening_element))))
              (target-pos (tsc-node-start-position opening-element)))
    (goto-char target-pos)))

;;;###autoload
(defun tsx-goto-tag-end ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (goto-char (tsc-node-end-position node))))

;;;###autoload
(defun tsx-goto-tag-beginning ()
  (interactive)
  (when-let ((node (save-excursion
                     (backward-char)
                     (tsx--tag-at-point))))
    (goto-char (tsc-node-start-position node))))

;;;###autoload
(defun tsx-goto-attribute-end ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (goto-char (tsc-node-end-position node))))

;;;###autoload
(defun tsx-goto-attribute-beginning ()
  (interactive)
  (when-let ((node (save-excursion
                     (backward-char)
                     (tsx--attribute-at-point))))
    (goto-char (tsc-node-start-position node))))

(defun tsx--goto-sibling (&optional backward)
  (or (> (if backward 0 (skip-chars-forward " \t\n\r")) 0)
      (let* ((node (tsx--highest-node-at-position (point)))
             (target (if backward
                         (tsc-get-prev-sibling node)
                       (tsc-get-next-sibling node))))
        (if target
            (goto-char (tsc-node-start-position target))
          (goto-char (if backward
                         (tsc-node-start-position node)
                       (tsc-node-end-position node)))))))

;;;###autoload
(defun tsx-goto-next-sibling ()
  (interactive)
  (tsx--goto-sibling))

;;;###autoload
(defun tsx-goto-prev-sibling ()
  (interactive)
  (tsx--goto-sibling t))

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

(defun tsx--tsc-children-of-type (node types)
  (let* ((index 0)
         (child-node (tsc-get-nth-child node index))
         (matching-children))
    (while child-node
      (when (memq (tsc-node-type child-node) types)
        (push child-node matching-children))
      (setq index (1+ index))
      (setq child-node (tsc-get-nth-child node index)))
    matching-children))

(defun tsx--highest-node-at-position (position)
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
         (node (tsx--highest-node-at-position curr-point))
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
         (below-node-start (> curr-line node-line))
         (in-node-body (and below-node-start
                            (< curr-line (car (tsc-node-end-point node)))))
         (target-column (cond
                         ((memq (tsc-node-type container) '(string template_string program)) 0)
                         ((and (eq node-type 'comment) below-node-start)
                          (+ container-column 1))
                         ((member node-type '("[" "(" "{" "}" ")" "]" "<" "/"
                                              jsx_closing_element
                                              statement_block
                                              else_clause))
                          (if in-node-body
                              (+ container-column js-indent-level)
                            container-column))
                         ((> curr-point (car (tsc-node-position-range container)))
                          (+ container-column js-indent-level))
                         (t curr-column))))
    (save-excursion (indent-line-to target-column))
    (skip-chars-forward " \t\n\r" (line-end-position))))

(defun tsx--unwrap-jsx-expressions-in-region (beg end)
  (when-let ((node (save-excursion (goto-char beg)
                                   (skip-chars-forward " \t\n\r")
                                   (tree-sitter-node-at-point 'jsx_expression))))
    (while (and node
                (<= (tsc-node-end-position node) end))
      (when (eq 'jsx_expression (tsc-node-type node))
        (tsx--replace-node
         node
         (let ((node-text (tsc-node-text node)))
           (concat
            " "
            (substring node-text 1 (1- (length node-text)))
            " "))))
      (setq node (tsc-get-next-sibling node)))))

(defun tsx--actual-content-region (beg end)
  "Returns the region with non-whitespace actual content between BEG and END"
  (let ((content-beg (save-excursion (goto-char beg) (skip-chars-forward " \t\n\r") (point)))
        (content-end (save-excursion (goto-char end) (skip-chars-backward " \t\n\r") (point))))
    (cons content-beg content-end)))

(defun tsx--comment-or-uncomment-jsx-region (beg end)
  ;; FIXME: doesn't work with nested jsx comments
  ;; currently not used
  (let* ((content-region (tsx--actual-content-region beg end))
         (content-beg (car content-region))
         (content-end (cdr content-region)))
    (replace-region-contents
     content-beg content-end
     (lambda () (concat comment-start
                   (buffer-substring-no-properties content-beg content-end)
                   comment-end)))))

(defun tsx--jsx-comment-only-p (beg end)
  (when-let ((content-region (tsx--actual-content-region beg end))
             (content-beg (car content-region))
             (node (save-excursion (goto-char content-beg)
                                   (tree-sitter-node-at-point 'jsx_expression)))
             (is-jsx-comment t))
    (while (and node is-jsx-comment (<= (tsc-node-end-position node) end))
      (setq is-jsx-comment (or (and (eq 'jsx_text (tsc-node-type node))
                                    (string-empty-p (string-trim (tsc-node-text node))))
                               (and (eq 'jsx_expression (tsc-node-type node))
                                    (= 3 (tsc-count-children node))
                                    (eq (tsc-node-type (tsc-get-nth-child node 1))
                                        'comment))))
      (setq node (tsc-get-next-sibling node)))
    is-jsx-comment))

;;;###autoload
(defun tsx-comment-or-uncomment-region (beg end)
  (evil-with-single-undo
    (let* ((content-region (tsx--actual-content-region beg end))
           (content-beg (car content-region))
           (outside-pos (1- content-beg))
           (outside-node-type (tsc-node-type (tsx--highest-node-at-position outside-pos)))
           (jsx-node-types '(jsx_element
                             jsx_opening_element
                             jsx_closing_element
                             jsx_text
                             jsx_attribute
                             jsx_fragment))
           (in-jsx (memq outside-node-type jsx-node-types))
           (comment-start (if in-jsx "{/*" "//"))
           (comment-end (if in-jsx "*/}" ""))
           ;; this must be true otherwise we get "Can't find comment end"
           ;; when we uncomment a JSX comment as the very first one in the buffer
           (comment-use-syntax t)
           (region-contains-only-jsx-comments (tsx--jsx-comment-only-p beg end)))
      (when region-contains-only-jsx-comments
        (tsx--unwrap-jsx-expressions-in-region beg end))
      (if (fboundp #'evilnc-comment-or-uncomment-region-internal)
          (evilnc-comment-or-uncomment-region-internal beg end)
        (comment-or-uncomment-region beg end)))
    (indent-region beg end)))

;;;###autoload
(defun tsx-element-vanish ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point t))
              (tag-nodes (tsx--element-tag-nodes element-node))
              (beg (tsc-node-start-position element-node))
              (end (tsc-node-end-position element-node)))
    (let ((closing-tag-node (car tag-nodes))
          (opening-tag-node (cadr tag-nodes)))
      (when closing-tag-node
        (tsx--node-delete closing-tag-node nil 'after))
      (when opening-tag-node
        (tsx--node-delete opening-tag-node nil 'before)))
    (indent-region beg end)))

(defun tsx--node-own-line (node &optional before-or-after)
  "Ensures NODE is on its own line and returns the number of new lines inserted
to achieve this."
  (let* ((beg (tsc-node-start-position node))
         (end (tsc-node-end-position node))
         (beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (linebreak-count 0))
    (when (or (eq before-or-after 'after)
              (not before-or-after))
      (save-excursion
        (goto-char end)
        (skip-chars-forward " \t\n\r")
        (when (= end-line (line-number-at-pos (point)))
          (insert "\n")
          (setq linebreak-count (1+ linebreak-count)))))
    (when (or (eq before-or-after 'before)
              (not before-or-after))
      (save-excursion
        (goto-char beg)
        (skip-chars-backward " \t\n\r")
        (when (= (line-number-at-pos (point)) beg-line)
          (insert "\n")
          (setq linebreak-count (1+ linebreak-count)))))
    linebreak-count))

;;;###autoload
(defun tsx-element-spread (only-this &optional dont-indent)
  (interactive "P")
  (when-let* ((element-node (tsx--element-at-point t))
              (tag-nodes (tsx--element-tag-nodes element-node))
              (beg (tsc-node-start-position element-node))
              (end (tsc-node-end-position element-node))
              (linebreak-count 0))
    (let ((closing-tag-node (car tag-nodes))
          (opening-tag-node (cadr tag-nodes))
          (child-elements (tsx--tsc-children-of-type
                           element-node
                           '(jsx_element jsx_self_closing_element))))
      (when closing-tag-node
        (setq linebreak-count
              (+ linebreak-count (tsx--node-own-line closing-tag-node))))
      (unless only-this
        (dolist (child-element child-elements)
          (setq
           linebreak-count
           (+ linebreak-count
              (save-excursion
                (goto-char (tsc-node-start-position child-element))
                (tsx-element-spread nil t))))))
      (when opening-tag-node
        (setq linebreak-count
              (+ linebreak-count (tsx--node-own-line opening-tag-node)))))
    (unless dont-indent
      (indent-region
       beg
       (save-excursion
         (goto-char (+ end linebreak-count))
         (skip-chars-forward " \t\n\r")
         (forward-char)
         (point))))
    linebreak-count))

;;;###autoload
(defun tsx-tag-spread ()
  (interactive)
  (when-let* ((tag-node (tsx--closest-parent-node nil '(jsx_opening_element jsx_closing_element jsx_self_closing_element)))
              (beg (tsc-node-start-position tag-node))
              (end (tsc-node-end-position tag-node))
              (linebreak-count 0))
    (let ((attribute-nodes (tsx--tsc-children-of-type
                           tag-node
                           '(jsx_attribute))))
      (dolist (attribute-node attribute-nodes)
        (setq
         linebreak-count
         (+ linebreak-count
            (tsx--node-own-line attribute-node 'before)))))
    (indent-region beg (+ end linebreak-count))
    linebreak-count))

(defun tsx--node-transpose (node &optional types)
  (let ((sibling (tsc-get-next-sibling node))
        (node-type (tsc-node-type node)))
    (while (and sibling
                (not (memq (tsc-node-type sibling)
                           (or types (list node-type)))))
      (setq sibling (tsc-get-next-sibling sibling)))
    (when sibling
      (let ((node-text (tsc-node-text node))
            (sibling-text (tsc-node-text sibling))
            (sibling-beg (tsc-node-start-position sibling)))
        (tsx--replace-node sibling node-text)
        (tsx--replace-node node sibling-text)
        (+ sibling-beg (- (length sibling-text) (length node-text)))))))

;;;###autoload
(defun tsx-element-transpose ()
  (interactive)
  (when-let ((node-target-types '(jsx_element jsx_self_closing_element))
             (node (or (tsx--closest-parent-node nil node-target-types)
                       (tsx--closest-parent-node (1- (point)) node-target-types))))
    (goto-char
     (or (evil-with-single-undo
           (tsx--node-transpose node node-target-types))
         (point)))))

;;;###autoload
(defun tsx-attribute-transpose ()
  (interactive)
  (when-let ((node-target-types '(jsx_attribute))
             (node (or (tsx--closest-parent-node nil node-target-types)
                       (tsx--closest-parent-node (1- (point)) node-target-types))))
    (goto-char
     (or (evil-with-single-undo
           (tsx--node-transpose node node-target-types))
         (point)))))
