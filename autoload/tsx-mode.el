;;; autoload/tsx-mode.el -*- lexical-binding: t; -*-

(require 'tree-sitter)

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
                          '(jsx_closing_element))))
    (cons
     (and opening-element
          (tsx--tsc-first-child-of-type
           opening-element
           '(identifier nested_identifier)))
     (and closing-element
          (tsx--tsc-first-child-of-type
           closing-element
           '(identifier nested_identifier))))))

(defun tsx--element-tag-name (element-node)
  (when-let* ((opening-element (tsx--tsc-first-child-of-type
                                element-node
                                '(jsx_opening_element)))
              (tag-name-node (car (tsx--element-tag-nodes element-node))))
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

(defun tsx--node-kill (node)
  (when-let* ((node-start (tsc-node-start-position node))
              (node-end (tsc-node-end-position node)))
    (kill-region node-start node-end)))

;;;###autoload
(defun tsx--evil-region-end-shim (pos)
  (if (or (bound-and-true-p evil-this-operator)
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
    (tsx--node-kill node)))

;;;###autoload
(defun tsx-tag-select ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-select node)))

;;;###autoload
(defun tsx-tag-kill ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-kill node)))

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

(defun tsx--goto-sibling (&optional backward)
  (or (> (if backward 0 (skip-chars-forward " \t\n")) 0)
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
    (skip-chars-forward " \t\n" (line-end-position))))

(defun tsx--unwrap-jsx-expressions-in-region (beg end)
  (when-let ((node (save-excursion (goto-char beg)
                                   (skip-chars-forward " \t\n")
                                   (tree-sitter-node-at-point 'jsx_expression))))
    (while (and node
                (<= (tsc-node-end-position node) end))
      (when (eq 'jsx_expression (tsc-node-type node))
        (replace-region-contents
         (tsc-node-start-position node)
         (tsc-node-end-position node)
         (lambda ()
           (let ((node-text (tsc-node-text node)))
             (concat
              " "
              (substring node-text 1 (1- (length node-text)))
              " ")))))
      (setq node (tsc-get-next-sibling node)))))

(defun tsx--actual-content-region (beg end)
  "Returns the region with non-whitespace actual content between BEG and END"
  (let ((content-beg (save-excursion (goto-char beg) (skip-chars-forward " \t\n") (point)))
        (content-end (save-excursion (goto-char end) (skip-chars-backward " \t\n") (point))))
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
