;;; autoload/rainbow-mode.el -*- lexical-binding: t; -*-

(require 'ov)

;;;###autoload
(defun my/rainbow-colorize-match-override (_orig-fun color &optional match)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
  (let* ((match (or match 0))
         (beg (match-beginning match))
         (end (match-end match)))
    ;; Add this chunk back if underlying text properties need to be set as a fallback
    ;; (a.k.a. normal rainbow-mode coloring)
    ;;
    ;; (put-text-property
    ;;  (match-beginning match) (match-end match)
    ;;  'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
    ;;                            "white" "black"))
    ;;          (:background ,color)))
    (ov-clear 'ovrainbow t beg end)
    (ov-clear 'hl-line t beg end)
    (ov
     beg end
     'face `((:foreground ,(if (< (rainbow-x-color-luminance color) 0.36)
                               "white" "black"))
             (:background ,color)
             (:weight bold))
     'ovrainbow t
     'priority 50
     'modification-hooks '(my/ov-evaporate-ovrainbow))))

(defun my/ov-evaporate-ovrainbow (_ov after beg end &optional _length)
  (let ((inhibit-modification-hooks t))
    (when after
      (ov-clear 'ovrainbow t beg end))
    (ov-clear 'hl-line)))

;;;###autoload
(defun my/rainbow-turn-off-override (_orig-fun)
    "Turn off rainbow-mode."
    (font-lock-remove-keywords
     nil
     `(,@rainbow-hexadecimal-colors-font-lock-keywords
       ,@rainbow-x-colors-font-lock-keywords
       ,@rainbow-latex-rgb-colors-font-lock-keywords
       ,@rainbow-r-colors-font-lock-keywords
       ,@rainbow-html-colors-font-lock-keywords
       ,@rainbow-html-rgb-colors-font-lock-keywords))
    (ov-clear 'ovrainbow))
