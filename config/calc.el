;;; config/calc.el -*- lexical-binding: t; -*-

(after! calc
  (defalias 'calcFunc-uconv 'math-convert-units))
