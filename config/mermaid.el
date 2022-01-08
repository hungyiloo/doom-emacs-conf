;;; config/mermaid.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(after! mermaid-mode
  (defun mermaid-compile ()
    "Compile the current mermaid file using mmdc."
    (interactive)
    (mermaid-compile-buffer))

  (defun mermaid-compile-buffer ()
    "Compile the current mermaid buffer using mmdc."
    (interactive)
    (let* ((tmp-file-name (concat mermaid-tmp-dir "current-buffer.mmd")))
      (when-let ((b (get-buffer "*mermaid-preview*")))
        (kill-buffer b))
      (write-region (point-min) (point-max) tmp-file-name)
      (let* ((input tmp-file-name)
             (output (concat (file-name-sans-extension input) mermaid-output-format)))
        (apply #'call-process mermaid-mmdc-location nil "*mmdc*" nil (append (split-string mermaid-flags " ") (list "-i" input "-o" output)))
        (let ((buffer (find-file-noselect output t)))
          (with-current-buffer buffer
            (rename-buffer "*mermaid-preview*" ))
          (display-buffer buffer)))))


  (set-popup-rule!
    "*mermaid-preview*"
    :width 0.5
    :side 'right
    :quit t))
