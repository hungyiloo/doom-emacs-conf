;;; config/js2-mode.el -*- lexical-binding: t; -*-

(after! (:or js2-mode json)
  ;; Fix some edge case javascript indenting
  (setq js-indent-level 2)

  ;; Fix some farty prettify-symbols-mode quirks in JavaScript.
  ;; Ligature fonts already handle =>, <= and >=
  ;; so I don't need emacs's prettification for them.
  (after! js
    (setq js--prettify-symbols-alist nil)))

(after! evil
  ;; Hook to install custom js/ts text objects
  ;; All functions below are autoloaded
  (add-hook! (javascript-mode js-mode js2-mode typescript-mode typescript-tsx-mode tsx-mode)
    (map! :map evil-operator-state-local-map
          "af" #'my/evil-outer-js-function
          "if" #'my/evil-inner-js-function
          "ad" #'my/evil-outer-js-declaration
          "id" #'my/evil-inner-js-declaration
          "as" #'my/evil-outer-js-statement
          "is" #'my/evil-inner-js-statement
          :map evil-visual-state-local-map
          "af" #'my/evil-visual-outer-js-function
          "if" #'my/evil-visual-inner-js-function
          "ad" #'my/evil-visual-outer-js-declaration
          "id" #'my/evil-visual-inner-js-declaration
          "as" #'my/evil-visual-outer-js-statement
          "is" #'my/evil-visual-inner-js-statement)))
