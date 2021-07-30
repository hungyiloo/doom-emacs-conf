;;; config/tsx-mode.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

(use-package! tsx-mode
  :commands tsx-mode
  :init
  (defun my/tsx-mode-setup ()
    ;; Doom uses `js2-line-break' and `js2-mode-extend-comment' in typescript-mode to extend comments
    ;; Without loading these, hitting RET in comments is broken
    ;; (autoload 'js2-mode-extend-comment "js2-mode")
    (require 'js2-mode)

    ;; Enable rainbow delimiters in tsx-mode
    (rainbow-delimiters-mode 1)

    ;; Enable emmet in tsx-mode
    (emmet-mode 1)

    ;; Enable lsp for tsx files
    (lsp!))

  ;; (advice-add #'js-syntax-propertize :around #'ignore)
  (add-hook! 'tsx-mode-hook
    (my/tsx-mode-setup))

  :config
  (map! :map 'tsx-mode-map
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
         :desc "Transpose" "t" #'tsx-element-transpose
         :desc "Clone" "c" #'tsx-element-clone
         :desc "Toggle self-closing" "%" #'tsx-element-toggle-self-closing)
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

  (after! editorconfig
    (add-to-list 'editorconfig-indentation-alist '(tsx-mode typescript-indent-level))))
