;;; lisp/journal.el -*- lexical-binding: t; -*-

(defun my-journal-time-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m-%d %a"
   (or timestamp (current-time))))

(defun my-journal-goto-heading (heading &optional post-heading-action)
  (interactive)
  (find-file "~/Notes/Journal.org")
  (doom/escape)
  (org-set-startup-visibility)
  (goto-char 0)
  (search-forward (concat "* " heading) nil t)
  (when post-heading-action (funcall post-heading-action))
  (recenter))

(let ((goto-first-heading (lambda () (interactive) (org-next-visible-heading 1))))
  (defun my-journal-goto-or-create-today ()
    (interactive)
    (find-file "~/Notes/Journal.org")
    (goto-char 0)
    (doom/escape)
    (org-set-startup-visibility)
    (let* ((today (my-journal-time-stamp))
           (today-posn (search-forward (concat "* " today) nil t)))
      (if (and (not today-posn) (search-forward (concat "* Daily Log") nil t))
          (progn
            (org-show-subtree)
            (org-next-visible-heading 1)
            (org-insert-heading)
            (org-move-subtree-up)
            (insert (my-journal-time-stamp))
            (org-insert-subheading nil))
        (funcall goto-first-heading)))
    (recenter))

  (defun my-journal-goto-exercise ()
    (interactive)
    (my-journal-goto-heading
     "Exercise"
     (lambda ()
       (interactive)
       (search-forward (number-to-string (nth 1 (calendar-current-date))) nil t))))

  (defun my-journal-goto-daily-log ()
    (interactive)
    (my-journal-goto-heading
     "Daily Log"
     goto-first-heading))

  (defun my-journal-goto-monthly-log ()
    (interactive)
    (my-journal-goto-heading
     "Monthly Log"
     goto-first-heading))

  (defun my-journal-goto-future-log ()
    (interactive)
    (my-journal-goto-heading
     "Future Log"
     goto-first-heading))

  (defun my-journal-goto-recurring ()
    (interactive)
    (my-journal-goto-heading
     "Recurring"
     goto-first-heading))

  (defun my-journal-goto-cook-list ()
    (interactive)
    (my-journal-goto-heading
     "Cook List"
     goto-first-heading)))

(map! :leader
      (:prefix-map ("j" . "journal")
       :desc "Today's Entry" "j" #'my-journal-goto-or-create-today
       :desc "Daily Log"     "d" #'my-journal-goto-daily-log
       :desc "Monthly Log"   "m" #'my-journal-goto-monthly-log
       :desc "Future Log"    "f" #'my-journal-goto-future-log
       :desc "Exercise"      "x" #'my-journal-goto-exercise
       :desc "Cook List"     "c" #'my-journal-goto-cook-list
       :desc "Recurring"     "r" #'my-journal-goto-recurring))

;; (defun diary-last-day-of-month (date)
;; "Return `t` if DATE is the last day of the month."
;;   (let* ((day (calendar-extract-day date))
;;          (month (calendar-extract-month date))
;;          (year (calendar-extract-year date))
;;          (last-day-of-month
;;             (calendar-last-day-of-month month year)))
;;     (= day last-day-of-month)))
