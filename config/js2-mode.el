;;; config/js2-mode.el -*- lexical-binding: t; -*-

(after! (:or js2-mode json)
  ;; Fix some edge case javascript indenting
  (setq js-indent-level 2)

  ;; Fix some farty prettify-symbols-mode quirks in JavaScript.
  ;; Ligature fonts already handle =>, <= and >=
  ;; so I don't need emacs's prettification for them.
  (after! js
    (setq js--prettify-symbols-alist nil))

  ;; Override this function to prevent lisp max-depth issues on hitting RET
  ;; within certain points inside comments.  This is because doom's
  ;; `+default--newline-indent-and-continue-comments-a' advice on
  ;; `newline-and-indent' can repeatedly call into `js2-line-break' since the
  ;; original version of it has the default `cond' case as `newline-and-indent'
  ;; too. Yuck.
  (defun js2-line-break (&optional _soft)
    "Break line at point and indent, continuing comment if within one.
If inside a string, and `js2-concat-multiline-strings' is not
nil, turn it into concatenation."
    (interactive)
    (let ((parse-status (syntax-ppss)))
      (cond
       ;; Check if we're inside a string.
       ((nth 3 parse-status)
        (if js2-concat-multiline-strings
            (js2-mode-split-string parse-status)
          (insert "\n")))
       ;; Check if inside a block comment.
       ((nth 4 parse-status)
        (js2-mode-extend-comment (nth 8 parse-status)))
       (t
        (newline)
        (funcall indent-line-function))))))

;; (after! evil
;;   ;; Hook to install custom js/ts text objects
;;   ;; All functions below are autoloaded
;;   (add-hook! (js-mode js-jsx-mode js2-mode typescript-mode typescript-tsx-mode tsx-mode)
;;     (map! :map evil-operator-state-local-map
;;           "af" #'my/evil-outer-js-function
;;           "if" #'my/evil-inner-js-function
;;           "ad" #'my/evil-outer-js-declaration
;;           "id" #'my/evil-inner-js-declaration
;;           "as" #'my/evil-outer-js-statement
;;           "is" #'my/evil-inner-js-statement
;;           :map evil-visual-state-local-map
;;           "af" #'my/evil-visual-outer-js-function
;;           "if" #'my/evil-visual-inner-js-function
;;           "ad" #'my/evil-visual-outer-js-declaration
;;           "id" #'my/evil-visual-inner-js-declaration
;;           "as" #'my/evil-visual-outer-js-statement
;;           "is" #'my/evil-visual-inner-js-statement)))
