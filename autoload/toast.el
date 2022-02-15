;; autoload/toast.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toast (title message)
  (message "%s: %s" title message)
  ;; (save-window-excursion
  ;;   (shell-command
  ;;    (concat
  ;;     "powershell.exe -ExecutionPolicy Bypass -File C:\\\\dev\\\\toast.ps1 -Title"
  ;;     " \""
  ;;     title
  ;;     "\" \""
  ;;     message
  ;;     "\" > /dev/null 2>&1 &")
  ;;    nil nil))
  )
