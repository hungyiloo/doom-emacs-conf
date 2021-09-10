;;; config/deft.el -*- lexical-binding: t; -*-

(after! deft
  (require 'org)
  (setq deft-directory org-roam-directory)
  (setq deft-use-filename-as-title nil)

  (map! :map deft-mode-map
        :ni "C-n" #'next-line
        :ni "C-p" #'previous-line
        :ni "C-u" #'evil-scroll-up
        :ni "C-d" #'evil-scroll-down
        :ni "<escape>" (cmd! (set-buffer-modified-p nil) (kill-current-buffer))
        :ni "C-<backspace>" #'deft-filter-clear)

  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)")))
