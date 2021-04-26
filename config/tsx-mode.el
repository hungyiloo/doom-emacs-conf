;;; config/tsx-mode.el -*- lexical-binding: t; -*-

(define-derived-mode tsx-mode js-jsx-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

(add-hook! 'after-init-hook
           ;; This derived mode won't work well until tree-sitter-indent
           ;; supports typescript/tsx.
           ;;
           ;; For now it provides a faster alternative when the web-mode derived
           ;; mode from doom is too slow
           (map! :map 'tsx-mode-map
                 :i "/" #'tsx-element-auto-close-maybe-h
                 :i "RET" #'tsx-newline-and-indent
                 :nv "]a" #'tsx-goto-next-sibling
                 :nv "[a" #'tsx-goto-prev-sibling)
           (map! :map 'tsx-mode-map
                 :localleader
                 (:prefix-map ("e" . "element")
                  :desc "Rename" "r" #'tsx-element-rename
                  :desc "Wrap" "w" #'tsx-element-wrap
                  :desc "End" "e" #'tsx-goto-element-end
                  :desc "Beginning" "b" #'tsx-goto-element-beginning
                  :desc "Select" "s" #'tsx-element-select
                  :desc "Select content" "a" #'tsx-element-select-content
                  :desc "Close" "/" #'tsx-element-close
                  :desc "Kill" "k" #'tsx-element-kill
                  :desc "Vanish" "v" #'tsx-element-vanish
                  :desc "Spread" "RET" #'tsx-element-spread
                  :desc "Transpose" "t" #'tsx-element-transpose)
                 (:prefix-map ("t" . "tag")
                  :desc "End" "e" #'tsx-goto-tag-end
                  :desc "Beginning" "b" #'tsx-goto-tag-beginning
                  :desc "Select" "s" #'tsx-tag-select
                  :desc "Kill" "k" #'tsx-tag-kill
                  :desc "Spread" "RET" #'tsx-tag-spread)
                 (:prefix-map ("a" . "attribute")
                  :desc "End" "e" #'tsx-goto-attribute-end
                  :desc "Beginning" "b" #'tsx-goto-attribute-beginning
                  :desc "Select" "s" #'tsx-attribute-select
                  :desc "Kill" "k" #'tsx-attribute-kill
                  :desc "Transpose" "t" #'tsx-attribute-transpose))

           (defun my/tsx-mode-setup ()
             (tree-sitter-require 'tsx)
             (tree-sitter-hl-add-patterns nil "[\"/\" \"*\"] @operator")
             (setq comment-region-function #'tsx-comment-region)
             (setq uncomment-region-function #'tsx-uncomment-region)
             ;; (add-to-list 'sp-pair-list (cons "/*" "*/"))
             (rainbow-delimiters-mode 1)
             (emmet-mode 1)
             (lsp)
             (setq-local indent-line-function #'tsx-indent-line-function))

           (add-hook! 'tsx-mode-hook
             (my/tsx-mode-setup)))

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))