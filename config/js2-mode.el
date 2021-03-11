;;; config/js2-mode.el -*- lexical-binding: t; -*-

(after! js2-mode
  ;; Fix some edge case javascript indenting
  (setq js-indent-level 2)

  ;; Fix some farty prettify-symbols-mode quirks in JavaScript.
  ;; Ligature fonts already handle =>, <= and >=
  ;; so I don't need emacs's prettification for them.
  (after! js
    (setq js--prettify-symbols-alist nil)))

(after! (:and evil (:or typescript-mode js2-mode))
  ;; Functions for JS/TS function text objects
  (let* ((fn-detect-pattern "[[:space:]\r\n]\\b[[:alnum:]]+?[[:space:]]*?(\\(.\\|[\n\r]\\)*?)\\([ \n\r]*?:\\(.\\|[\n\r]\\)*?\\)?[[:space:]]*?{")
         (lambda-detect-pattern "\\((.*?)\\|\\b[[:alnum:]]+?\\)\\([ \n\r]*?:\\(.\\|[\n\r]\\)*?\\)?[[:space:]\r\n]=>[[:space:]\r\n]*?{?")
         (pattern (concat "\\(" fn-detect-pattern "\\|" lambda-detect-pattern "\\)"))
         (keywords '("break" "as" "any" "switch"
                     "case" "if" "throw" "else"
                     "var" "number" "string" "get"
                     "module" "type" "instanceof" "typeof"
                     "public" "private" "enum" "export"
                     "finally" "for" "while" "void"
                     "null" "super" "this" "new"
                     "in" "return" "true" "false"
                     "any" "extends" "static" "let"
                     "package" "implements" "interface"
                     "new" "try" "yield" "const"
                     "continue" "do" "catch")))
    (require 'smartparens)
    (defun my/evil-select-inner-javascript-function (type visual-p)
      (or (save-excursion
            (when (and (doom-region-active-p)
                       (> (point) (mark)))
              (let ((prev-point (point))
                    (prev-mark (mark)))
                (set-mark prev-point)
                (goto-char prev-mark)))
            (let ((origin (point))
                  (current-range))
              (condition-case nil
                  (progn
                    (while (or (not current-range)
                               (not (<= (car current-range) origin (cadr current-range))))
                      (when current-range (goto-char (car current-range)))
                      (let* ((function-beg (progn
                                             (if (doom-region-active-p)
                                                 (progn
                                                   (backward-char)
                                                   (search-backward-regexp "[^ \n\r=>]"))
                                               (progn
                                                 (search-forward "=>" (line-end-position) t)
                                                 (search-forward "{" (line-end-position) t)))
                                             (search-backward-regexp pattern)
                                             (while (save-excursion
                                                      (forward-char)
                                                      (member (thing-at-point 'symbol t) keywords))
                                               (search-backward-regexp pattern))
                                             (point)))
                             (beg (progn
                                    (goto-char function-beg)
                                    (search-forward-regexp pattern)
                                    (when (member (char-after) '(?\n ?\t ? ))
                                      (search-forward-regexp "[^ ]")
                                      (backward-char))
                                    (when (eq ?{ (char-before))
                                      (backward-char))
                                    (point)))
                             (end (progn
                                    (goto-char beg)
                                    (when (eq ?{ (char-after))
                                      (forward-char))
                                    (sp-end-of-sexp)
                                    (when (eq ?} (char-after))
                                      (forward-char))
                                    (if visual-p
                                        (1- (point))
                                      (point)))))
                        (setq current-range (evil-range beg end type))))
                    current-range)
                (error nil))))
          (error "No surrounding function found")))
    (defun my/evil-select-outer-javascript-function (type visual-p)
      (or (save-excursion
            (when (and (doom-region-active-p)
                       (> (point) (mark)))
              (let ((prev-point (point))
                    (prev-mark (mark)))
                (set-mark prev-point)
                (goto-char prev-mark)))
            (let ((origin (point))
                  (current-range))
              (condition-case nil
                  (progn
                    (while (or (not current-range)
                               (not (<= (car current-range) origin (cadr current-range))))
                      (when current-range (goto-char (car current-range)))
                      (let* ((function-beg (progn
                                             (unless (or current-range (doom-region-active-p))
                                               (search-forward "=>" (line-end-position) t)
                                               (search-forward "{" (line-end-position) t))
                                             (search-backward-regexp pattern)
                                             (while (save-excursion
                                                      (forward-char)
                                                      (member (thing-at-point 'symbol t) keywords))
                                               (search-backward-regexp pattern))
                                             (point)))
                             (beg (progn
                                    (goto-char function-beg)
                                    (when (member (char-after) '(? ?\t ?\r ?\n))
                                      (forward-char))
                                    (search-backward "function" (line-beginning-position) t)
                                    (search-backward-regexp "\\(private\\|public\\)" (line-beginning-position) t)
                                    (search-backward "export" (line-beginning-position) t)
                                    (point)))
                             (end (progn
                                    (goto-char function-beg)
                                    (search-forward-regexp pattern)
                                    (when (member (char-after) '(?\n ?\t ? ))
                                      (search-forward-regexp "[^ ]")
                                      (backward-char))
                                    (when (eq ?{ (char-after))
                                      (sp-down-sexp))
                                    (sp-end-of-sexp)
                                    (when (eq ?} (char-after))
                                      (forward-char))
                                    (when visual-p
                                      (backward-char))
                                    (point))))
                        (setq current-range (evil-range beg end type))))
                    current-range)
                (error nil))))
          (error "No surrounding function found"))))

  (evil-define-text-object
    evil-visual-inner-javascript-function (count &optional beg end type)
    "Visual inner text object for all Javascript functions."
    (my/evil-select-inner-javascript-function type t))
  (evil-define-text-object
    evil-inner-javascript-function (count &optional beg end type)
    "Inner text object for all Javascript functions."
    (my/evil-select-inner-javascript-function type nil))
  (evil-define-text-object
    evil-visual-outer-javascript-function (count &optional beg end type)
    "Visual outer text object for all Javascript functions."
    (my/evil-select-outer-javascript-function type t))
  (evil-define-text-object
    evil-outer-javascript-function (count &optional beg end type)
    "Outer text object for all Javascript functions."
    (my/evil-select-outer-javascript-function type nil))

  ;; Hook to install the above functions
  (add-hook! (javascript-mode js-mode js2-mode typescript-mode)
    (map! :map evil-operator-state-local-map
          "af" #'evil-outer-javascript-function
          "if" #'evil-inner-javascript-function
          :map evil-visual-state-local-map
          "af" #'evil-visual-outer-javascript-function
          "if" #'evil-visual-inner-javascript-function)))
