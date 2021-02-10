;;; config/editorconfig.el -*- lexical-binding: t; -*-

(after! editorconfig
  (setcdr (assq 'web-mode editorconfig-indentation-alist)
          '((web-mode-indent-style lambda (size) 2)
            ;; I prefer the web mode attr indent behavior when it's set to nil
            ;;
            ;; <a href="http://google.com"
            ;;    target="_blank">See how the attributes line up vertically?</a>
            ;;
            ;; web-mode-attr-indent-offset
            ;; web-mode-attr-value-indent-offset

            web-mode-code-indent-offset
            web-mode-css-indent-offset
            web-mode-markup-indent-offset
            web-mode-sql-indent-offset
            web-mode-block-padding
            web-mode-script-padding
            web-mode-style-padding)))
