;;; autoload/consult.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/consult-color (&optional initial)
    (interactive)
    (let* ((cmd "curl https://www.colourlovers.com/api/colors?numResults=50&format=json \
                 -H \"user-agent: Emacs\" \
                 --data-urlencode \"keywords=ARG\"")
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
                    :initial (consult--async-split-initial initial)
                    :require-match t
                    :sort nil)))
      (when (region-active-p) (delete-active-region))
      (insert (funcall get-key result))))

;;;###autoload
(defun my/consult-palette (&optional initial)
    (interactive)
    (let* ((cmd "curl https://www.colourlovers.com/api/palettes?numResults=50&format=json \
                 -H \"user-agent: Emacs\" \
                 --data-urlencode \"keywords=ARG\"")
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
                    :initial (consult--async-split-initial initial)
                    :require-match t
                    :sort nil)))
      (when (region-active-p)
        (delete-active-region))
      (insert (funcall get-key result))))

;;;###autoload
(defun my/widen-dwim ()
  (interactive)
  (consult-focus-lines t)
  (widen))
