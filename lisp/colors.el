;;; lisp/colors.el -*- lexical-binding: t; -*-

(use-package! kurecolor
  :config

  ;; Redefine this function to specifically detect rgba/hex color "symbols"
  (defun kurecolor-replace-current (fn &rest args)
    "Get the current unspaced string at point. Replace with the return value of the function FN with ARGS."
    (let* ((search-range (max (- (point) (line-beginning-position))
                              (- (line-end-position) (point))))
           (bounds (if (and transient-mark-mode mark-active)
                       (list (region-beginning) (region-end))
                     (when (or (thing-at-point-looking-at
                                "#[0-9a-f]\\{6,8\\}"
                                search-range)
                               (thing-at-point-looking-at
                                "rgba?(\s*\\(,?\s*[0-9]\\{1,3\\}\\)\\{3\\}\\(,\s*[0-9]+\.?[0-9]*\\)?\s*)"
                                search-range))
                       (list (match-beginning 0) (match-end 0)))))
           (excerpt (apply #'buffer-substring-no-properties bounds))
           (change (car args))
           (replacement (if args
                            (funcall fn excerpt change)
                          ;; no args
                          (funcall fn excerpt))))

      (apply #'delete-region bounds)
      (insert replacement)))

  (let ((get-alpha (lambda (hex) (when (= (length hex) 9) (substring hex 7 9)))))
    (defun kurecolor-adjust-brightness (hex amount)
      "Adjust the HEX color brightness by AMOUNT 0.0-0.1."
      (concat
       (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
         (setq val (min 1.0 (+ amount val)))
         (kurecolor-rgb-to-hex
          (kurecolor-hsv-to-rgb hue sat val)))
       (funcall get-alpha hex)))

    (defun kurecolor-adjust-saturation (hex amount)
      "Adjust the HEX color saturation by AMOUNT 0.0-0.1."
      (concat
       (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
         (setq sat (min 1.0 (+ sat amount)))
         (kurecolor-rgb-to-hex
          (kurecolor-hsv-to-rgb hue sat val)))
       (funcall get-alpha hex)))

    (defun kurecolor-adjust-hue (hex amount)
      "Adjust the HEX color hue by AMOUNT 0.0-0.1."
      (concat
       (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
         (setq hue (mod (+ hue amount) 1.0))
         (kurecolor-rgb-to-hex
          (kurecolor-hsv-to-rgb hue sat val)))
       (funcall get-alpha hex)))

    (defun kurecolor-adjust-alpha (hex amount)
      "Adjust the HEX color alpha by AMOUNT 0.0-0.1."
      (let ((alpha (string-to-number (or (funcall get-alpha hex) "00") 16)))
        (concat
         (substring hex 0 7) (format "%x" (mod (+ alpha amount) 256))))))

  (defun kurecolor-increase-alpha-by-step (x)
    "Increase alpha on hex color at point (or in region) by step.
Accepts universal argument (X)."
    (interactive "P")
    (unless (numberp x) (setq x 1))
    (kurecolor-replace-current
     'kurecolor-adjust-alpha
     (/ (* x kurecolor-color-adjust-brightness-step) 100.0)))

  (defun kurecolor-decrease-alpha-by-step (x)
    "Decrease alpha on hex color at point (or in region) by step.
Accepts universal argument (X)."
    (interactive "P")
    (unless (numberp x) (setq x 1))
    (kurecolor-replace-current
     'kurecolor-adjust-alpha
     (/ (* (* -1 x) kurecolor-color-adjust-brightness-step) 100.0)))

  (defhydra my-kurecolor-hydra
    (:color pink
     :hint nil
     :pre (progn
            ;; Makes sure hl-line-mode is off, color is in hex format, and opens the kurecolor hydra
            (hl-line-mode -1)
            (or (css--named-color-to-hex)
                (css--rgb-to-named-color-or-hex))))
    "
Inc/Dec      _w_/_W_ brightness      _d_/_D_ saturation      _e_/_E_ hue      _a_/_A_ alpha      "
    ("w" kurecolor-decrease-brightness-by-step)
    ("W" kurecolor-increase-brightness-by-step)
    ("d" kurecolor-decrease-saturation-by-step)
    ("D" kurecolor-increase-saturation-by-step)
    ("e" kurecolor-decrease-hue-by-step)
    ("E" kurecolor-increase-hue-by-step)
    ("a" kurecolor-increase-alpha-by-step)
    ("A" kurecolor-increase-alpha-by-step)
    ("q" nil "cancel" :color blue))

  (map! :map (css-mode-map sass-mode-map stylus-mode-map)
        :localleader
        (:prefix ("c" . "colors")
         "c" #'css-cycle-color-format
         "k" #'my-kurecolor-hydra/body)))
