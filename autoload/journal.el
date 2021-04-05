;;; autoload/journal.el -*- lexical-binding: t; -*-

(require 'org)

(defun my/journal-date-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m-%d %a"
   (or timestamp (current-time))))

(defun my/journal-goto-heading (heading &optional post-heading-action)
  (interactive)
  (evil-set-jump)
  (find-file +org-capture-journal-file)
  (olivetti-mode 1)
  (let* ((result nil)
         (position (save-excursion
                     (org-remove-occur-highlights)
                     (widen)
                     (org-set-startup-visibility)
                     (goto-char 0)
                     (search-forward-regexp (concat "* " (regexp-quote heading) "$") nil t)
                     (setq result (when post-heading-action (funcall post-heading-action)))
                     (point))))
    ;; (scroll-on-jump (goto-char position))
    (goto-char position)
    (recenter)
    result))

;;;###autoload
(defun my/journal-goto-or-create-today ()
    (interactive)
    (my/journal-goto-heading
     "Daily"
     (lambda ()
       (interactive)
       (let* ((today (my/journal-date-stamp))
              (today-posn (search-forward (concat "* " today) nil t)))
         (if (not today-posn)
             (let ((entry-count (length (org-map-entries
                                         nil
                                         ;; count the number of direct subheadings at point
                                         (concat "LEVEL=" (number-to-string (+ (org-outline-level) 1)))
                                         'tree))))
               (org-insert-subheading nil)
               (insert today)
               ;; move the new entry to the top of all its siblings
               (when (> entry-count 0) (org-move-subtree-up entry-count))
               t)
           nil)))))

;;;###autoload
(defun my/journal-goto-or-create-today-excursion ()
  (interactive)
  (save-window-excursion
    (my/journal-goto-or-create-today)))

;;;###autoload
(defun my/journal-goto-or-create-today-subheading ()
  (interactive)
  (let ((today-was-created (my/journal-goto-or-create-today)))
    (my/journal-goto-heading
     (my/journal-date-stamp)
     (lambda ()
       (interactive)
       (if today-was-created
           (org-insert-subheading nil)
         (org-next-visible-heading 1))))))

;;;###autoload
(defun my/journal-goto-exercise ()
  (interactive)
  (my/journal-goto-heading
   "Exercise"
   (lambda ()
     (interactive)
     (search-forward-regexp (concat "[MTWFS] " (number-to-string (nth 1 (calendar-current-date)))) nil t))))

;;;###autoload
(defun my/journal-goto-daily-log ()
  (interactive)
  (my/journal-goto-heading
   "Daily"
   (lambda ()  (org-next-visible-heading 1))))

;;;###autoload
(defun my/journal-goto-monthly-log ()
  (interactive)
  (my/journal-goto-heading
   "Monthly"
   (lambda () (org-next-visible-heading 1))))

;;;###autoload
(defun my/journal-goto-future-log ()
  (interactive)
  (my/journal-goto-heading
   "Future"
   (lambda () (org-next-visible-heading 1))))

;;;###autoload
(defun my/journal-goto-recurring ()
  (interactive)
  (my/journal-goto-heading
   "Recurring"
   (lambda () (org-next-visible-heading 1))))

;;;###autoload
(defun my/journal-goto-cook-list ()
  (interactive)
  (my/journal-goto-heading
   "Cooking"
   (lambda () (org-next-visible-heading 1))))



;; (defun diary-last-day-of-month (date)
;; "Return `t` if DATE is the last day of the month."
;;   (let* ((day (calendar-extract-day date))
;;          (month (calendar-extract-month date))
;;          (year (calendar-extract-year date))
;;          (last-day-of-month
;;             (calendar-last-day-of-month month year)))
;;     (= day last-day-of-month)))
