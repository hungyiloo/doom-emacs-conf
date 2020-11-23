;;; lisp/journal.el -*- lexical-binding: t; -*-

(defun my-journal-date-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m-%d %a"
   (or timestamp (current-time))))

(defun my-journal-month-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m"
   (or timestamp (current-time))))

(defun my-journal-goto-heading (heading this-month &optional post-heading-action)
  (interactive)
  (evil-set-jump)
  (find-file "~/Notes/Journal.org")
  (org-remove-occur-highlights)
  (widen)
  (org-set-startup-visibility)
  (goto-char 0)
  (when this-month
    (search-forward
     (concat "* " (my-journal-month-stamp))
     nil
     t))
  (search-forward (concat "* " heading) nil t)
  (when post-heading-action (funcall post-heading-action))
  (recenter))

(let ((goto-first-heading (lambda () (org-next-visible-heading 1))))

  (defun my-journal-goto-or-create-today ()
    (interactive)
    (my-journal-goto-heading
     "Daily Log"
     t
     (lambda ()
       (interactive)
       (let* ((today (my-journal-date-stamp))
              (today-posn (search-forward (concat "* " today) nil t)))
         (if (not today-posn)
             (progn
               (org-show-subtree)
               (org-next-visible-heading 1)
               (org-insert-heading)
               (org-move-subtree-up)
               (insert (my-journal-date-stamp))
               (org-insert-subheading nil))
           (funcall goto-first-heading))))))

  (defun my-journal-goto-exercise ()
    (interactive)
    (my-journal-goto-heading
     "Exercise"
     t
     (lambda ()
       (interactive)
       (search-forward-regexp (concat "[MTWFS] " (number-to-string (nth 1 (calendar-current-date)))) nil t))))

  (defun my-journal-goto-daily-log ()
    (interactive)
    (my-journal-goto-heading
     "Daily Log"
     t
     goto-first-heading))

  (defun my-journal-goto-monthly-log ()
    (interactive)
    (my-journal-goto-heading
     "Monthly Log"
     t
     goto-first-heading))

  (defun my-journal-goto-future-log ()
    (interactive)
    (my-journal-goto-heading
     "Future Log"
     t
     goto-first-heading))

  (defun my-journal-goto-recurring ()
    (interactive)
    (my-journal-goto-heading
     "Recurring"
     nil
     goto-first-heading))

  (defun my-journal-goto-cook-list ()
    (interactive)
    (my-journal-goto-heading
     "Cook List"
     nil
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
