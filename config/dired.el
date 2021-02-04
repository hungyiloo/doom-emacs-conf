(after! dired
  (defun my-dired-duplicate-marked-files ()
    (interactive)
    (dired-do-copy-regexp "\\([^\\.]*\\)\\.\\(.*\\)" "\\1#.\\2"))
  (map! :after dired
        :map dired-mode-map
        :n "|" #'my-dired-duplicate-marked-files)
  ;; Uncomment this block to prevent dired creating
  ;; lots of buffers when navigating through files/dirs
  ;; (map! :map dired-mode-map
  ;;       :n "RET" #'dired-find-alternate-file
  ;;       :desc "dired-up-directory (alt)" :n "^" (lambda () (interactive) (find-alternate-file "..")))
  )
