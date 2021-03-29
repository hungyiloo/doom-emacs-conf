;;; config/rainbow-mode.el -*- lexical-binding: t; -*-

(after! rainbow-mode
  ;; Enhance rainbow-mode to use overlays instead of text properties.
  ;; This prevents inteferefence with hl-line-mode.
  ;; Source: https://github.com/amosbird/rainbow-mode
  (require 'ov)
  (defun rainbow-colorize-match (color &optional match)
    "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
    (let ((match (or match 0)))
      (ov-clear (match-beginning match) (match-end match) 'ovrainbow t)
      (ov
       (match-beginning match) (match-end match)
       'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                 "white" "black"))
               (:background ,color))
       'ovrainbow t
       'priority 5000)))
  (defun rainbow-turn-off ()
    "Turn off rainbow-mode."
    (font-lock-remove-keywords
     nil
     `(,@rainbow-hexadecimal-colors-font-lock-keywords
       ,@rainbow-x-colors-font-lock-keywords
       ,@rainbow-latex-rgb-colors-font-lock-keywords
       ,@rainbow-r-colors-font-lock-keywords
       ,@rainbow-html-colors-font-lock-keywords
       ,@rainbow-html-rgb-colors-font-lock-keywords))
    (ov-clear 'ovrainbow)))
