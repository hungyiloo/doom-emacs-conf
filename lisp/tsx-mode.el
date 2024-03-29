;;; lisp/tsx-mode.el --- Real support for TSX -*- lexical-binding: t; -*-

(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'seq)
(require 'cl-lib)

(add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx))

(defvar tsx-indent-level 2
  "Number of spaces for each indentation level in tsx-mode")

(defgroup tsx-mode nil
  "Support for TSX."
  :group 'tsx-mode)

;;;###autoload
(define-derived-mode tsx-mode prog-mode "TSX"
  "Major mode for editing TSX files."
  :lighter ":TSX"
  :group 'tsx-mode
  (modify-syntax-entry ?' "'")
  (modify-syntax-entry ?= ".")
  (modify-syntax-entry ?> ".")
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?+ ".")
  (modify-syntax-entry ?- ".")
  (modify-syntax-entry ?/ ". 124")
  (modify-syntax-entry ?* ". 23b")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?` "\"")
  (tree-sitter-require 'tsx)
  (tree-sitter-hl-add-patterns nil "[\"/\" \"*\"] @operator")
  (tree-sitter-hl-add-patterns nil "(jsx_text) @string")
  (setq-local comment-region-function #'tsx-comment-region)
  (setq-local uncomment-region-function #'tsx-uncomment-region)
  (setq-local indent-line-function #'tsx-indent-line-function)
  (setq-local comment-line-break-function #'tsx-comment-line-break)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "[[:space:]]*\\(//+\\|{?/\\*+\\)")
  (setq-local comment-end "")
  (setq-local comment-end-skip "\\(\\*+/}?[[:space:]]*\\)\n?\\|\n"))

(advice-add #'self-insert-command
            :around
            (defun tsx-self-insert-command-advice (orig-fun N &optional C)
              (if (and (eq major-mode 'tsx-mode)
                       (eq C ?/))
                  (tsx-element-auto-close-maybe-h)
                (funcall orig-fun N C))))

(defmacro tsx-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (if (fboundp 'with-undo-amalgamate)
      `(with-undo-amalgamate ,@body)
    `(let ((marker (prepare-change-group)))
       (unwind-protect ,@body
         (undo-amalgamate-change-group marker)))))

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

(defvar tsx-mode-enable-auto-closing t "Enabled auto closing JSX tags tsx-mode")

(defun tsx-element-auto-close-maybe-h ()
  (interactive)
  (if (and tsx-mode-enable-auto-closing
           (eq (char-before) ?<))
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
  (tsx--closest-parent-node nil (list 'jsx_element 'jsx_fragment
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

(defun tsx-element-rename ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point t))
              (tag-nodes (tsx--element-tag-name-nodes element-node))
              (first-tag-node (car tag-nodes))
              (tag-name (tsc-node-text first-tag-node))
              (new-tag-name (string-trim (read-string "Rename element: " tag-name))))
    (unless (> (length new-tag-name) 0) (user-error "Oops! That isn't a valid tag name..."))
    (tsx-with-single-undo
     (dolist (tag-node tag-nodes)
       (tsx--replace-node tag-node new-tag-name)))))

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

(defun tsx--node-delete (node &optional push-kill clean-whitespace dont-reindent)
  (when-let ((node-region (tsx--node-region node clean-whitespace)))
    (funcall (if push-kill #'kill-region #'delete-region)
             (car node-region) (cdr node-region))
    (unless dont-reindent (funcall indent-line-function))))

(defun tsx--evil-region-end-shim (pos)
  (if (or (and (boundp 'evil-state) (eq evil-state 'operator))
          (and (fboundp #'evil-visual-state-p) (evil-visual-state-p)))
      pos (1- pos)))

(defun tsx-element-select ()
  (interactive)
  (when-let ((node (tsx--element-at-point t)))
    (tsx--node-select node)))

(defun tsx-element-kill ()
  (interactive)
  (when-let ((node (tsx--element-at-point t)))
    (tsx--node-delete node t 'after)))

(defun tsx-element-toggle-self-closing ()
  (interactive)
  (when-let* ((node (tsx--element-at-point t))
              (tag-name (tsx--element-tag-name node)))
    (if (eq (tsc-node-type node) 'jsx_self_closing_element)
        (tsx--replace-node
         node
         (concat (substring (tsc-node-text node) 0 -2)
                 ">"
                 "</" tag-name ">"))
      (tsx--replace-node
       node
       (concat (substring (tsc-node-text (car (last (tsx--element-tag-nodes node)))) 0 -1)
               "/>")))))

(defun tsx-tag-select ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-select node)))

(defun tsx-tag-kill ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (tsx--node-delete node t 'after)))

(defun tsx-attribute-select ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (tsx--node-select node)))

(defun tsx-attribute-kill ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (tsx--node-delete node t 'after)))

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

(defun tsx-goto-element-end ()
  (interactive)
  (when-let* ((node (tsx--element-at-point t))
              (closing-element (or (and (eq (tsc-node-type node) 'jsx_self_closing_element) node)
                                   (and (eq (tsc-node-type node) 'jsx_fragment) node)
                                   (tsx--tsc-first-child-of-type node '(jsx_closing_element))))
              (target-pos (tsc-node-end-position closing-element)))
    (goto-char target-pos)))

(defun tsx-goto-element-beginning ()
  (interactive)
  (when-let* ((node (save-excursion
                      (backward-char)
                      (tsx--element-at-point t)))
              (opening-element (or (and (eq (tsc-node-type node) 'jsx_self_closing_element) node)
                                   (and (eq (tsc-node-type node) 'jsx_fragment) node)
                                   (tsx--tsc-first-child-of-type node '(jsx_opening_element))))
              (target-pos (tsc-node-start-position opening-element)))
    (goto-char target-pos)))

(defun tsx-goto-tag-end ()
  (interactive)
  (when-let ((node (tsx--tag-at-point)))
    (goto-char (tsc-node-end-position node))))

(defun tsx-goto-tag-beginning ()
  (interactive)
  (when-let ((node (save-excursion
                     (backward-char)
                     (tsx--tag-at-point))))
    (goto-char (tsc-node-start-position node))))

(defun tsx-goto-attribute-end ()
  (interactive)
  (when-let ((node (tsx--attribute-at-point)))
    (goto-char (tsc-node-end-position node))))

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

(defun tsx-goto-next-sibling ()
  (interactive)
  (tsx--goto-sibling))

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
    (let* ((current-node (tree-sitter-node-at-pos))
           (parent-node (tsc-get-parent current-node)))
      (while (and parent-node
                  (not (eq 'program (tsc-node-type parent-node)))
                  (eq (tsc-node-start-byte parent-node)
                      (tsc-node-start-byte current-node)))
        (setq current-node parent-node)
        (setq parent-node (tsc-get-parent current-node)))
      current-node)))

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
                              (+ container-column tsx-indent-level)
                            container-column))
                         ((> curr-point (car (tsc-node-position-range container)))
                          (+ container-column tsx-indent-level))
                         (t curr-column))))
    (save-excursion (indent-line-to target-column))
    (when (= 0 (current-column))
      (skip-chars-forward " \t\n\r" (line-end-position)))))

(defun tsx--unwrap-jsx-expressions-in-region (beg end)
  (when-let ((node (save-excursion (goto-char beg)
                                   (skip-chars-forward " \t\n\r")
                                   (tree-sitter-node-at-pos 'jsx_expression))))
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
                                   (tree-sitter-node-at-pos 'jsx_expression)))
             (is-jsx-comment nil))
    (while (and node is-jsx-comment (<= (tsc-node-end-position node) end))
      (setq is-jsx-comment (or (and (eq 'jsx_text (tsc-node-type node))
                                    (string-empty-p (string-trim (tsc-node-text node))))
                               (and (eq 'jsx_expression (tsc-node-type node))
                                    (= 3 (tsc-count-children node))
                                    (eq (tsc-node-type (tsc-get-nth-child node 1))
                                        'comment))))
      (setq node (tsc-get-next-sibling node)))
    is-jsx-comment))

(defun tsx-comment-region (beg end &optional ARG)
  (tsx-comment-or-uncomment-region beg end 'comment ARG))

(defun tsx-uncomment-region (beg end &optional ARG)
  (tsx-comment-or-uncomment-region beg end 'uncomment ARG))

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
         ;; (is-ts-mode (derived-mode-p 'typescript-mode))
         (comment-use-syntax (if in-jsx nil t)))
    ;; (when is-ts-mode (tsx--unwrap-jsx-expressions-in-region beg end))
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

(defun tsx-element-vanish ()
  (interactive)
  (when-let* ((element-node (tsx--element-at-point t))
              (tag-nodes (tsx--element-tag-nodes element-node))
              (beg (tsc-node-start-position element-node))
              (end (tsc-node-end-position element-node)))
    (let ((closing-tag-node (car tag-nodes))
          (opening-tag-node (cadr tag-nodes)))
      (when closing-tag-node
        (tsx--node-delete closing-tag-node nil 'after t))
      (when opening-tag-node
        (tsx--node-delete opening-tag-node nil 'before t)))
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

(defun tsx-element-transpose ()
  (interactive)
  (when-let ((node-target-types '(jsx_element jsx_self_closing_element jsx_expression))
             (node (or (tsx--closest-parent-node nil node-target-types)
                       (tsx--closest-parent-node (1- (point)) node-target-types))))
    (goto-char
     (or (tsx-with-single-undo
          (tsx--node-transpose node node-target-types))
         (point)))))

(defun tsx-attribute-transpose ()
  (interactive)
  (when-let ((node-target-types '(jsx_attribute))
             (node (or (tsx--closest-parent-node nil node-target-types)
                       (tsx--closest-parent-node (1- (point)) node-target-types))))
    (goto-char
     (or (tsx-with-single-undo
          (tsx--node-transpose node node-target-types))
         (point)))))

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

(defun tsx-comment-line-break (&optional _soft)
  "Break line at point and indent, continuing comment if within one.
If inside a string, and `js2-concat-multiline-strings' is not
nil, turn it into concatenation."
  (interactive)
  (let ((parse-status (syntax-ppss)))
    (cond
     ;; Check if inside a block comment.
     ((nth 4 parse-status)
      (tsx-extend-comment (nth 8 parse-status)))
     (t
      (insert "\n")
      (tsx-indent-line-function)))))

(defun tsx-extend-comment (start-pos)
  "Indent the line and, when inside a comment block, add comment prefix."
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (when (< (point) start-pos)
        (goto-char start-pos))
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    ;; Heuristic for whether we need to close the comment:
    ;; if we've got a parse error here, assume it's an unterminated
    ;; comment.
    (setq needs-close
          (or
           (get-char-property (1- (point)) 'js2-error)
           ;; The heuristic above doesn't work well when we're
           ;; creating a comment and there's another one downstream,
           ;; as our parser thinks this one ends at the end of the
           ;; next one.  (You can have a /* inside a js block comment.)
           ;; So just close it if the next non-ws char isn't a *.
           (and first-line
                (eolp)
                (save-excursion
                  (skip-chars-forward " \t\r\n")
                  (not (eq (char-after) ?*))))))
    (delete-horizontal-space)
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     (single
      (indent-to col)
      (insert "// ")))
    ;; Don't need to extend the comment after all.
    (tsx-indent-line-function)))

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

(provide 'tsx-mode)
