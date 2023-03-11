;;; config/typescript-mode.el -*- lexical-binding: t; -*-

(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq tide-native-json-parsing t)
  (setq tide-completion-ignore-case t)
  (require 'js2-mode))

(after! ob-typescript
  (defun org-babel-execute:typescript (body params)
    "Execute a block of Typescript code with org-babel.  This function is
called by `org-babel-execute-src-block'"
    (let* ((tmp-src-file (org-babel-temp-file "ts-src-" ".ts"))
           (tmp-out-file (org-babel-temp-file "ts-src-" ".js"))
           (cmdline (cdr (assoc :cmdline params)))
           (cmdline (if cmdline (concat " " cmdline) ""))
           (jsexec (if (assoc :wrap params) ""
                     (concat " ; node " (org-babel-process-file-name tmp-out-file))
                     )))
      (with-temp-file tmp-src-file (insert (org-babel-expand-body:generic
                                            body params (org-babel-variable-assignments:typescript params))))
      (let ((results (org-babel-eval (format "tsc %s --target esnext -lib esnext,dom -out %s %s %s"
                                             cmdline
                                             (org-babel-process-file-name tmp-out-file)
                                             (org-babel-process-file-name tmp-src-file)
                                             jsexec)
                                     ""))
            (jstrans (with-temp-buffer
                       (insert-file-contents tmp-out-file)
                       (buffer-substring-no-properties (point-min) (point-max))
                       )))
        (if (eq jsexec "") jstrans results)
        ))))
