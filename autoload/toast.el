;; autoload/toast.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toast (title message)
  (with-temp-buffer
    (shell-command
     (concat
      "powershell.exe -ExecutionPolicy Bypass -File ~/.config/doom/scripts/toast.ps1 -Title"
      " \""
      title
      "\" \""
      message
      "\" > /dev/null 2>&1")
     t)))
