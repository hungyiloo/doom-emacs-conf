;;; config/tsx-mode.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

(use-package! tsx-mode
  :commands tsx-mode
  :init
  (defun my/tsx-mode-setup ()
    ;; Enable rainbow delimiters in tsx-mode
    (rainbow-delimiters-mode 1)

    ;; Enable emmet in tsx-mode
    (emmet-mode 1)

    ;; Integration with evil embrace/surround
    ;; i.e. fix angle bracket behavior
    (setq-local evil-embrace-evil-surround-keys '(40 91 123 41 93 125 34 39 98 66 116 27 119 87 115 112 60 62))

    ;; Better rainbow delimiters integration
    (setq-local rainbow-delimiters-pick-face-function #'tsx-rainbow-delimiters-pick-face)

    ;; evil-mc doesn't play nice with tag autoclosing,
    ;; so disable it while in multi-cursor mode
    (add-hook 'evil-mc-before-cursors-created
              (defun tsx-disable-autoclosing-before-cursors-created ()
                (when (eq major-mode 'tsx-mode)
                  (setq-local tsx-mode--auto-closing-temporarily-disabled t)
                  (setq-local tsx-mode-enable-auto-closing nil))))
    (add-hook 'evil-mc-after-cursors-deleted
              (defun tsx-reenable-autoclosing-after-cursors-deleted ()
                (when (and (eq major-mode 'tsx-mode)
                           (bound-and-true-p tsx-mode--auto-closing-temporarily-disabled))
                  (setq-local tsx-mode-enable-auto-closing t))))

    (after! 'evil-mc
      (add-to-list 'evil-mc-custom-known-commands `(tsx-newline-and-indent (:default . evil-mc-execute-call))))

    ;; evil-nc (commenting) integration
    (setq-local evilnc-comment-or-uncomment-region-function 'tsx-comment-or-uncomment-region)

    ;; Add spelling support
    (require 'spell-fu)
    (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:comment)
    (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:doc)
    (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:string)
    (add-to-list 'spell-fu-faces-exclude 'tree-sitter-hl-face:embedded)

    ;; Enable lsp for tsx files
    (lsp!)
    (setq-local lsp-enable-indentation nil))

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

  (after! evilnc
    ;; Register tsx-mode as a cpp-like mode for commenting purposes
    (add-to-list 'evilnc-cpp-like-comment-syntax-modes 'tsx-mode))

  (after! editorconfig
    (add-to-list 'editorconfig-indentation-alist '(tsx-mode tsx-indent-level)))

  (after! smartparens
    (add-to-list 'sp-pairs '(tsx-mode
                             (:open "<" :close ">" :actions
                              (wrap autoskip navigate)
                              :when
                              (:add)
                              :unless
                              (:add)
                              :pre-handlers
                              (:add)
                              :post-handlers
                              (:add))
                             (:open "/*" :close "*/" :actions
                              (insert)
                              :when
                              (:add)
                              :unless
                              (:add)
                              :pre-handlers
                              (:add)
                              :post-handlers
                              (("| " "SPC")
                               (" | " "*")
                               ("|[i]\n[i]" "RET")))
                             (:open "<!--" :close "-->" :actions
                              (insert)
                              :when
                              (:add)
                              :unless
                              (sp-point-before-word-p sp-point-before-same-p)
                              :pre-handlers
                              (:add)
                              :post-handlers
                              (("| " "SPC")))))))

;; Enable doom tree-sitter support to tsx-mode
(add-hook! '(tsx-mode-local-vars-hook) :append #'tree-sitter!)
;; Add tree-sitter support for my custom tsx-mode
(after! evil-textobj-tree-sitter
        (pushnew! evil-textobj-tree-sitter-major-mode-language-alist '(tsx-mode . "tsx")))
(after! tree-sitter
        (pushnew! tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))
