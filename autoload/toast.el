;; autoload/toast.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toast (title message)
  ;; (message "%s: %s" title message)
  (save-window-excursion
    (let ((async-shell-command-buffer 'new-buffer)
          (async-shell-command-display-buffer nil))
      (shell-command
       (concat
        "powershell.exe -WindowStyle hidden -ExecutionPolicy Bypass -File C:\\\\dev\\\\toast.ps1 -Title"
        " \""
        title
        "\" \""
        message
        "\" > /dev/null 2>&1 &")
       nil nil))))
