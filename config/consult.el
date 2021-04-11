;;; config/consult.el -*- lexical-binding: t; -*-

(use-package! consult
  :config
  ;; Adjust some keybindings to use consult equivalents
  (map! :leader
        "," #'+compres/consult-project-buffer
        "<" #'consult-buffer
        (:prefix-map ("M" . "mode")
         "M" #'consult-mode-command
         "N" #'consult-minor-mode-menu)
        (:prefix-map ("s" . "search")
         "I" #'consult-project-imenu
         "S" #'+compres/consult-line-symbol-at-point)
        (:prefix-map ("i" . "insert")
         "c" #'my/consult-color
         "C" #'my/consult-palette))

  ;; Make occur mode (with consult) act more similarly
  ;; to wgrep mode
  (map! :map occur-mode-map
        :n "C-x C-p" nil
        :n "C-c C-p" #'occur-edit-mode)
  (map! :map occur-edit-mode-map
        :n "C-x C-q" nil
        :n "C-c C-k" #'occur-cease-edit
        :n "Z Z" #'occur-cease-edit)

  (map! :map narrow-map
        "l" #'consult-focus-lines
        "w" (defun my/widen-dwim ()
              (interactive)
              (consult-focus-lines t)
              (widen)))

  (defun my/consult-color (&optional initial)
    (interactive)
    (let* ((cmd "curl https://www.colourlovers.com/api/colors?numResults=50&format=json --data-urlencode \"keywords=ARG\"")
           (get-key
            (lambda (color) (format "#%s" (downcase (alist-get 'hex color)))))
           (get-candidate
            (lambda (color)
              (let ((hex (funcall get-key color)))
                (cons (format "%s %s %s"
                              (propertize "████" 'face `(:foreground ,hex))
                              (propertize hex 'face 'consult-key)
                              (alist-get 'title color))
                      color))))
           (result (consult--read
                    (consult--async-command cmd
                      (consult--async-transform
                       (lambda (response)
                         (mapcar
                          get-candidate
                          (json-read-from-string (car response))))))
                    :prompt "Color keywords: "
                    :lookup #'consult--lookup-cdr
                    :initial (concat consult-async-default-split initial)
                    :require-match t
                    :sort nil)))
      (when (region-active-p) (delete-active-region))
      (insert (funcall get-key result))))

  (defun my/consult-palette (&optional initial)
    (interactive)
    (let* ((cmd "curl https://www.colourlovers.com/api/palettes?numResults=50&format=json --data-urlencode \"keywords=ARG\"")
           (get-hex-colors
            (lambda (palette)
              (mapcar
               (lambda (hex) (format "#%s" (downcase hex)))
               (alist-get 'colors palette))))
           (get-key
            (lambda (palette) (string-join (funcall get-hex-colors palette) " ")))
           (get-candidate
            (lambda (palette)
              (let* ((colors (funcall get-hex-colors palette)))
                (cons (format "%-10s %s %s"
                              (string-join (mapcar
                                            (lambda (c) (propertize "██" 'face `(:foreground ,c)))
                                            colors))
                              (propertize (funcall get-key palette) 'face 'consult-key)
                              (alist-get 'title palette))
                      palette))))
           (result (consult--read
                    (consult--async-command cmd
                      (consult--async-transform
                       (lambda (response)
                         (mapcar
                          get-candidate
                          (json-read-from-string (car response))))))
                    :prompt "Palette keywords: "
                    :lookup #'consult--lookup-cdr
                    :initial (concat consult-async-default-split initial)
                    :require-match t
                    :sort nil)))
      (when (region-active-p)
        (delete-active-region))
      (insert (funcall get-key result)))))
