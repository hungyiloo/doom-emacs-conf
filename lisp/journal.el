;;; lisp/journal.el -*- lexical-binding: t; -*-

(defun my/journal-date-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m-%d %a"
   (or timestamp (current-time))))

(defun my/journal-month-stamp (&optional timestamp)
  (format-time-string
   "%Y-%m"
   (or timestamp (current-time))))

(defun my/journal-goto-heading (heading this-month &optional post-heading-action)
  (interactive)
  (evil-set-jump)
  (find-file +org-capture-journal-file)
  (let* ((result nil)
         (position (save-excursion
                     (org-remove-occur-highlights)
                     (widen)
                     (org-set-startup-visibility)
                     (goto-char 0)
                     (when this-month
                       (search-forward
                        (concat "* " (my/journal-month-stamp))
                        nil
                        t))
                     (search-forward (concat "* " heading) nil t)
                     (setq result (when post-heading-action (funcall post-heading-action)))
                     (point))))
    ;; (scroll-on-jump (goto-char position))
    (goto-char position)
    (recenter)
    result))

(defun my/journal-goto-or-create-today ()
    (interactive)
    (my/journal-goto-heading
     "Daily Log"
     t
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

(defun my/journal-goto-or-create-today-excursion ()
  (interactive)
  (save-window-excursion
    (my/journal-goto-or-create-today)))

(let ((goto-first-heading (lambda () (org-next-visible-heading 1))))

  (defun my/journal-goto-or-create-today-subheading ()
    (interactive)
    (let ((today-was-created (my/journal-goto-or-create-today)))
      (my/journal-goto-heading
       (my/journal-date-stamp)
       t
       (lambda ()
         (interactive)
         (if today-was-created
             (org-insert-subheading nil)
           (org-next-visible-heading 1))))))

  (defun my/journal-goto-exercise ()
    (interactive)
    (my/journal-goto-heading
     "Exercise"
     t
     (lambda ()
       (interactive)
       (search-forward-regexp (concat "[MTWFS] " (number-to-string (nth 1 (calendar-current-date)))) nil t))))

  (defun my/journal-goto-daily-log ()
    (interactive)
    (my/journal-goto-heading
     "Daily Log"
     t
     goto-first-heading))

  (defun my/journal-goto-monthly-log ()
    (interactive)
    (my/journal-goto-heading
     "Monthly Log"
     t
     goto-first-heading))

  (defun my/journal-goto-future-log ()
    (interactive)
    (my/journal-goto-heading
     "Future Log"
     t
     goto-first-heading))

  (defun my/journal-goto-recurring ()
    (interactive)
    (my/journal-goto-heading
     "Recurring"
     nil
     goto-first-heading))

  (defun my/journal-goto-cook-list ()
    (interactive)
    (my/journal-goto-heading
     "Cook List"
     nil
     goto-first-heading)))

(map! "<f6>" #'org-capture
      :leader
      (:prefix-map ("j" . "journal")
       :desc "Today's Entry" "j" #'my/journal-goto-or-create-today-subheading
       :desc "Daily Log"     "d" #'my/journal-goto-daily-log
       :desc "Monthly Log"   "m" #'my/journal-goto-monthly-log
       :desc "Future Log"    "f" #'my/journal-goto-future-log
       :desc "Exercise"      "x" #'my/journal-goto-exercise
       :desc "Cook List"     "c" #'my/journal-goto-cook-list
       :desc "Recurring"     "r" #'my/journal-goto-recurring))

;; (defun diary-last-day-of-month (date)
;; "Return `t` if DATE is the last day of the month."
;;   (let* ((day (calendar-extract-day date))
;;          (month (calendar-extract-month date))
;;          (year (calendar-extract-year date))
;;          (last-day-of-month
;;             (calendar-last-day-of-month month year)))
;;     (= day last-day-of-month)))

(use-package! org
  :init
  (setq +org-capture-journal-file "~/Notes/Journal.org")
  :defer t
  :config
  (setq org-capture-templates
        `(("j" "Journal")
          ("jj" "Journal Today")
          ("jjt" "Journal Today Todo" entry
           (file+function +org-capture-journal-file my/journal-goto-or-create-today-excursion)
           "* TODO %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jjn" "Journal Today Note" entry
           (file+function +org-capture-journal-file my/journal-goto-or-create-today-excursion)
           "* %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jm" "Journal Monthly")
          ("jmt" "Journal Monthly Todo" entry
           (file+headline +org-capture-journal-file "Monthly Log")
           "* TODO %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jmn" "Journal Monthly Note" entry
           (file+headline +org-capture-journal-file "Monthly Log")
           "* %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jf" "Journal Future")
          ("jft" "Journal Future Todo" entry
           (file+headline +org-capture-journal-file "Future Log")
           "* TODO %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jfn" "Journal Future Note" entry
           (file+headline +org-capture-journal-file "Future Log")
           "* %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jc" "Journal Cook List")
          ("jct" "Journal Cook List Todo" entry
           (file+headline +org-capture-journal-file "Cook List")
           "* TODO %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          ("jcn" "Journal Cook List Note" entry
           (file+headline +org-capture-journal-file "Cook List")
           "* %?"
           :kill-buffer t
           :empty-lines-before 1
           :empty-lines-after 1)
          )))
