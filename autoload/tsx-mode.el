;;; autoload/tsx-mode.el -*- lexical-binding: t; -*-

(require 'tree-sitter)
(require 'seq)
(require 'cl-lib)

;;;###autoload
(defun tsx-element-close (&optional dont-indent)
  (interactive)
  (let ((restore-angle-bracket (looking-at-p "<>")))
    (when restore-angle-bracket (save-excursion (forward-char) (delete-char 1)))
    (or (when-let* ((nearest-container (tsx--closest-parent-node (1- (point)) '(jsx_element ERROR)))
                    (tag-name (tsx--element-tag-name nearest-container))
                    (closing-tag-markup (format "</%s>" tag-name)))
          (insert closing-tag-markup)
          (unless dont-indent
            (funcall indent-line-function))
          t)
        (save-excursion (forward-char) (insert ">") nil))))

;;;###autoload
(defun tsx-element-auto-close-maybe-h ()
  (interactive)
  (if (eq (char-before) ?<)
      (let* ((before-tag-pos (- (point) 1))
             (empty-tag (eq (char-before before-tag-pos) ?>)))
        (or (when (save-excursion (backward-char) (tsx-element-close t))
              (delete-char -1)
              (when empty-tag (goto-char before-tag-pos))
              (funcall indent-line-function)
              t)
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
  (when-let* ((tag-name-node (car (last (tsx--element-tag-name-nodes element-node)))))
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

(defun tsx--node-region (node &optional include-whitespace)
  (let ((node-start (tsc-node-start-position node))
        (node-end (tsc-node-end-position node)))
    (cons
     (if (or (eq include-whitespace t)
             (eq include-whitespace 'before))
         (save-excursion
           (goto-char node-start)
           (skip-chars-backward " \t\n\r")
           (point))
       node-start)
     (if (or (eq include-whitespace t)
             (eq include-whitespace 'after))
         (save-excursion
           (goto-char node-end)
           (skip-chars-forward " \t\n\r")
           (point))
       node-end))))

(defun tsx--node-delete (node &optional push-kill clean-whitespace)
  (when-let ((node-region (tsx--node-region node clean-whitespace)))
    (funcall (if push-kill #'kill-region #'delete-region)
             (car node-region) (cdr node-region))))

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
                  (not (eq 'program (tsc-node-type parent-node)))
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
    (when (= 0 (current-column))
      (skip-chars-forward " \t\n\r" (line-end-position)))))

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
(defun tsx-comment-region (beg end &optional ARG)
  (tsx-comment-or-uncomment-region beg end 'comment ARG))

;;;###autoload
(defun tsx-uncomment-region (beg end &optional ARG)
  (tsx-comment-or-uncomment-region beg end 'uncomment ARG))

;;;###autoload
(defun tsx-comment-or-uncomment-region (beg end &optional explicit ARG)
  (interactive)
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
         (comment-start-skip (if in-jsx
                                 "{\\(?://+\\|/\\*+\\)\\s *"
                               comment-start-skip))
         (comment-end-skip (if in-jsx
                               "[ 	]*\\(\\s>\\|\\*+/}\\)"
                             comment-end-skip))
         (comment-use-syntax nil))
    (save-excursion
      (cond
       ((eq explicit 'comment) (comment-region-default beg end ARG))
       ((eq explicit 'uncomment) (uncomment-region-default beg end ARG))
       (t (if (or
               (and in-jsx (tsx--jsx-comment-only-p beg end))
               (comment-only-p beg end))
              (uncomment-region-default beg end ARG)
            (comment-region-default beg end ARG))))))
  (indent-region beg end))

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
  (cl-letf (((symbol-function #'js-syntax-propertize) #'ignore))
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
      linebreak-count)))

;;;###autoload
(defun tsx-tag-spread ()
  (interactive)
  (cl-letf (((symbol-function #'js-syntax-propertize) #'ignore))
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
      linebreak-count)))

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
  (when-let ((node-target-types '(jsx_element jsx_self_closing_element jsx_expression))
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

;;;###autoload
(defun tsx-element-clone ()
  (interactive)
  (when-let* ((node (tsx--element-at-point t))
              (node-region (tsx--node-region node 'after))
              (beg (car node-region))
              (end (cdr node-region)))
    (goto-char end)
    (save-excursion
      (insert (buffer-substring-no-properties beg end))
      (indent-region beg (+ end (- end beg))))
    (skip-chars-forward " \t\n\r")))

;;;###autoload
(defun tsx-newline-and-indent (&optional ARG)
  (interactive "P")
  (cond
   ;; smartparens-like auto element spreading
   ;; on newline inside empty tag
   ((and (looking-at-p "</")
         (eq (char-before) ?>))
    (newline-and-indent)
    (newline-and-indent ARG)
    (forward-line -1)
    (end-of-line)
    (funcall indent-line-function))

   ;; Multiline comment indentation helper
   ;; REVIEW: is this needed if we fix smartparens commenting?
   ((looking-at-p " ?\\*/")
    (newline-and-indent)
    (funcall indent-line-function))

   (t (newline-and-indent ARG))))

;;;###autoload
(defun tsx-rainbow-delimiters-pick-face (depth match loc)
  "Return a face name appropriate for nesting depth DEPTH.
DEPTH and MATCH are as in `rainbow-delimiters-pick-face-function'.

The returned value is either `rainbow-delimiters-unmatched-face',
`rainbow-delimiters-mismatched-face', or one of the
`rainbow-delimiters-depth-N-face' faces, obeying
`rainbow-delimiters-max-face-count' and
`rainbow-delimiters-outermost-only-face-count'."
  (cond
   ((<= depth 0)
    'rainbow-delimiters-unmatched-face)
   ((not match)
    'rainbow-delimiters-mismatched-face)
   ;; Don't do rainbow delimiters for HTML tag angle brackets
   ((memq (char-after loc) '(?< ?>))
    'tree-sitter-hl-face:operator)
   (t
    (intern-soft
     (concat "rainbow-delimiters-depth-"
             (number-to-string
              (if (<= depth rainbow-delimiters-max-face-count)
                  ;; Our nesting depth has a face defined for it.
                  depth
                ;; Deeper than # of defined faces; cycle back through to
                ;; `rainbow-delimiters-outermost-only-face-count' + 1.
                ;; Return face # that corresponds to current nesting level.
                (+ 1 rainbow-delimiters-outermost-only-face-count
                   (mod (- depth rainbow-delimiters-max-face-count 1)
                        (- rainbow-delimiters-max-face-count
                           rainbow-delimiters-outermost-only-face-count)))))
             "-face")))))

;; (defun tsx-syntax-propertize (start end)
;;   ;; JavaScript allows immediate regular expression objects, written /.../.
;;   (goto-char start)
;;   (if js-jsx-syntax (remove-text-properties start end js-jsx--text-properties))
;;   (js-syntax-propertize-regexp end)
;;   (funcall
;;    (syntax-propertize-rules
;;     ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
;;     ;; FIXME: Allow regexps after infix ops like + ...
;;     ;; https://developer.mozilla.org/en/JavaScript/Reference/Operators
;;     ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
;;     ;; point I think only * and / would be missing which could also be added,
;;     ;; but need care to avoid affecting the // and */ comment markers.
;;     ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
;;      (1 (ignore
;; 	 (forward-char -1)
;;          (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
;;                    ;; If the / is at the beginning of line, we have to check
;;                    ;; the end of the previous text.
;;                    (save-excursion
;;                      (goto-char (match-beginning 0))
;;                      (forward-comment (- (point)))
;;                      (memq (char-before)
;;                            (eval-when-compile (append "=({[,:;" '(nil))))))
;;            (put-text-property (match-beginning 1) (match-end 1)
;;                               'syntax-table (string-to-syntax "\"/"))
;;            (js-syntax-propertize-regexp end)))))
;;     ("\\`\\(#\\)!" (1 "< b"))
;;     ("<" (0 (ignore
;;              (when js-jsx-syntax
;;                ;; Not inside a comment or string.
;;                (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
;;                  (js-jsx--syntax-propertize-tag end)))))))
;;    (point) end))
