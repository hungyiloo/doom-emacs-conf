;;; config/calc.el -*- lexical-binding: t; -*-

(after! calc
  (defalias 'calcFunc-uconv 'math-convert-units))

(after! literate-calc-mode
  ;; Fix case sensitive reserved word ignoring in literate calc variable
  ;; bindings that interfere with variable names like "Working Day" and
  ;; "Internet Usage"
  (advice-add #'literate-calc--substitute-variable-values
              :around
              (defun my/literate-calc--subsitute-variable-values (orig-fun &rest args)
                (let ((case-fold-search nil))
                  (apply orig-fun args)))))
