;;; config/journal.el -*- lexical-binding: t; -*-

(use-package! org
  :init
  (setq +org-capture-journal-file "~/Notes/Journal.org")
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
