(after! scss-mode
  (defun my-scss-mode-setup ()
    ;; Uncomment this section to use asterisk style comments in SCSS
    ;; (setq comment-start "/* "
    ;;       comment-end " */")
    )
  (add-hook! 'scss-mode-hook #'my-scss-mode-setup))
