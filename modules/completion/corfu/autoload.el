;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-complete-file-at-point ()
  "Complete a file path from scratch at point"
  (interactive)
  (completion-in-region (point) (point) #'read-file-name-internal))

;;;###autoload
(defun +corfu-files ()
  "Complete using files source"
  (interactive)
  (let ((completion-at-point-functions (list #'+file-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dabbrev ()
  "Complete using dabbrev source"
  (interactive)
  (let ((completion-at-point-functions (list #'+dabbrev-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-ispell ()
  "Complete using ispell source.

See `ispell-lookup-words' for more info"
  (interactive)
  (let ((completion-at-point-functions (list #'+ispell-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dict ()
  "Complete using dict source.

See `+dict--words' for extra words, and `+dict-file' for a wordslist source "
  (interactive)
  (let ((completion-at-point-functions (list #'+dict-completion-at-point-function)))
    (completion-at-point)))


(require 'dabbrev)

;;;###autoload
(defun +file-completion-at-point-function ()
  "File name completion-at-point-function."
  (when-let (bounds (bounds-of-thing-at-point 'filename))
    (list (car bounds) (cdr bounds)
          'read-file-name-internal
          :exclusive 'no
          :annotation-function (lambda (_) " (File)"))))

;;;###autoload
(defun +dabbrev-completion-at-point-function ()
  (let ((dabbrev-check-all-buffers nil)
        (dabbrev-check-other-buffers nil))
    (dabbrev--reset-global-variables))
  (let ((abbrev (ignore-errors (dabbrev--abbrev-at-point))))
    (when (and abbrev (not (string-match-p "[ \t]" abbrev)))
      (pcase ;; Interruptible scanning
          (while-no-input
            (let ((inhibit-message t)
                  (message-log-max nil))
              (or (dabbrev--find-all-expansions
                   abbrev (dabbrev--ignore-case-p abbrev))
                  t)))
        ('nil (keyboard-quit))
        ('t nil)
        (words
         ;; Ignore completions which are too short
         (let ((min-len (+ 4 (length abbrev))))
           (setq words (seq-remove (lambda (x) (< (length x) min-len)) words)))
         (when words
           (let ((beg (progn (search-backward abbrev) (point)))
                 (end (progn (search-forward abbrev) (point))))
             (unless (string-match-p "\n" (buffer-substring beg end))
               (list beg end words
                     :exclusive 'no
                     :annotation-function (lambda (_) " (Dabbrev)"))))))))))

(autoload 'ispell-lookup-words "ispell")

;;;###autoload
(defun +ispell-completion-at-point-function ()
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (table (with-demoted-errors
                         (let ((message-log-max nil)
                               (inhibit-message t))
                           (ispell-lookup-words
                            (format "*%s*"
                                    (buffer-substring-no-properties (car bounds) (cdr bounds))))))))
    (list (car bounds) (cdr bounds) table
          :exclusive 'no
          :annotation-function (lambda (_) " (Ispell)"))))

(defun +word-completion-at-point-function (words)
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds) words
          :exclusive 'no
          :annotation-function (lambda (_) " (Words)"))))

(defvar +dict--words nil)
(defvar +dict-file "/etc/dictionaries-common/words")

;;;###autoload
(defun +dict-completion-at-point-function ()
  (+word-completion-at-point-function
   (or +dict--words
       (setq +dict--words
             (split-string (with-temp-buffer
                             (insert-file-contents-literally +dict-file)
                             (buffer-string))
                           "\n")))))
