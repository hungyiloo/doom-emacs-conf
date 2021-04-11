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
    (let ((hex (consult--read
                (consult--async-command
                    "curl https://www.colourlovers.com/api/colors?numResults=50&format=json --data-urlencode \"keywords=ARG\""
                  (consult--async-transform
                   (lambda (response)
                     (mapcar
                      (lambda (color)
                        (let ((hex (format "#%s" (downcase (alist-get 'hex color)))))
                          (cons (format "%s %s %s"
                                        (propertize "████" 'face `(:foreground ,hex))
                                        (propertize hex 'face 'consult-key)
                                        (alist-get 'title color))
                                hex)))
                      (json-read-from-string (car response))))))
                :prompt "Color keywords: "
                :lookup #'consult--lookup-cdr
                :initial (concat consult-async-default-split initial)
                :require-match t
                :sort nil)))
      (when (region-active-p)
        (delete-active-region))
      (insert hex)))

  (defun my/consult-palette (&optional initial)
    (interactive)
    (let ((result (consult--read
                   (consult--async-command
                       "curl https://www.colourlovers.com/api/palettes?numResults=50&format=json --data-urlencode \"keywords=ARG\""
                     (consult--async-transform
                      (lambda (response)
                        (mapcar
                         (lambda (palette)
                           (let* ((colors (mapcar
                                           (lambda (c) (format "#%s" (downcase c)))
                                           (alist-get 'colors palette)))
                                  (result (string-join colors " ")))
                             (cons (format "%-10s %s %s"
                                           (string-join (mapcar (lambda (c) (propertize "██" 'face `(:foreground ,c))) colors))
                                           (propertize result 'face 'consult-key)
                                           (alist-get 'title palette))
                                   result)))
                         (json-read-from-string (car response))))))
                   :prompt "Palette keywords: "
                   :lookup #'consult--lookup-cdr
                   :initial (concat consult-async-default-split initial)
                   :require-match t
                   :sort nil)))
      (when (region-active-p)
        (delete-active-region))
      (insert result))))
