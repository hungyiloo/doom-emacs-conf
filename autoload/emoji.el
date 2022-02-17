;;; autoload/symbol.el -*- lexical-binding: t; -*-

;;;###autoload
(defun emoji-insert-dwim ()
  (interactive)
  (let ((emojis (mapcar
                 (lambda (emoji) (concat (symbol-name (car emoji)) " - " (cdr emoji)))
                  (json-read-file (concat doom-private-dir "/emoji.json")))))
    (insert (car (split-string (completing-read "Find and insert symbol: " emojis nil t) " ")))))
