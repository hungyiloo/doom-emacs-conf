;;; lisp/charge.el -*- lexical-binding: t; -*-

(require 'org)
(require 'ox)
(require 'f)
(require 'seq)
(require 'subr-x)

(defun charge-site (&rest options)
  (let (site routes option-key)
    ;; Split up options into site site and routes
    (dolist (x options)
      (cond ((and x (keywordp x)) (setq option-key x))
            (option-key (push (cons option-key x) site)
                        (setq option-key nil))
            (t (push x routes))))

    (let ((base-url (or (alist-get :base-url site) ""))
          (output (or (alist-get :output site) "output"))
          (urls (make-hash-table :test #'equal)))

      ;; Hash all IDs to canonical URLs
      (dolist (route routes)
        (let ((url (alist-get :url route)))
          (dolist (particle (alist-get :particles route))
            (puthash
             (alist-get :id particle)
             (concat
              base-url
              (if (functionp url)
                  (funcall url particle)
                url))
             urls))))
      (setf (alist-get :urls site) urls)
      (setq charge--site site)

      ;; Emit all routes and their particles
      (dolist (route routes)
        (let ((pathfinder (alist-get :path route))
              (particles (alist-get :particles route))
              (emitter (alist-get :emit route)))
          (setq charge--route route)
          (dolist (particle particles)
            (setq charge--particle particle)
            (let* ((paths (if (functionp pathfinder) (funcall pathfinder particle) pathfinder))
                   (paths (if (listp paths) paths (list paths)))
                   (destinations (mapcar (lambda (path) (f-join output path)) paths)))
              (dolist (destination destinations)
                (mkdir (file-name-directory destination) t)
                (funcall emitter destination particle route site))))))

      ;; Clean up
      (setq charge--particle nil)
      (setq charge--route nil)
      (setq charge--site nil))))

(defun charge-route (particle-or-particles &rest options)
  (declare (indent defun))
  (let ((route-alist (mapcar (lambda (x) (apply #'cons x))
                             (seq-partition options 2))))
    (setf (alist-get :particles route-alist)
          (if (charge-particle-p particle-or-particles)
              (list particle-or-particles) ; support a single particle as input to a route
            particle-or-particles))
    route-alist))

(defun charge-particle (id &rest data)
  (declare (indent defun))
  `((:particle . t)
    (:id . ,id)
    ,@(mapcar (lambda (x) (apply #'cons x))
              (seq-partition data 2))))

(defun charge-particle-p (object)
  (alist-get :particle object))

(defun charge-url (site particle-or-id)
  (let ((urls (alist-get :urls site)))
    (if (listp particle-or-id)
        (gethash (alist-get :id particle-or-id) urls)
      (gethash particle-or-id urls))))

(defun charge-html (&rest template)
  (let (tag attr-name (content (list)) (attrs (list)))
    (mapc
     (lambda (x)
       (cond ((and x (listp x))
              (push (apply #'charge-html x) content))
             ((and (not tag) x (symbolp x))
              (setq tag x))
             ((keywordp x)
              (setq attr-name x))
             (attr-name
              (push (cons attr-name x) attrs)
              (setq attr-name nil))
             (t
              (unless (null x) (push (format "%s" x) content)))))
     template)
    (let ((tag-is-void (charge--tag-is-void tag)))
      (concat
       (when tag
         (thread-last attrs
                      (nreverse)
                      (mapcar
                       (lambda (attr)
                         (format
                          (if (cdr attr) " %s=\"%s\"" " %s")
                          (substring (symbol-name (car attr)) 1) (cdr attr))))
                      (apply #'concat)
                      (format
                       (if tag-is-void "<%s%s/>" "<%s%s>")
                       tag)))
       (unless tag-is-void
         (thread-last content
                      (nreverse)
                      (apply #'concat)))
       (when (and tag (not tag-is-void))
         (format "</%s>" tag))))))

(defun charge-prettify-html (html)
  "Reformats HTML to make it readable by adding newlines where necessary."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward ">\\s-*<" (point-max) t)
      (replace-match ">\n<" t t))
    (html-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun charge-write (text path)
  (write-region text nil path))

(defun charge-format (format-string particle-key)
  (lambda (particle)
    (format format-string (alist-get particle-key particle))))

(defun charge--tag-is-void (tag)
  (memq tag '(area base br col embed hr img input link meta param source track wbr)))

(defcustom charge-org-keywords '("slug" "title" "date" "draft" "filetags" "description")
  "The supported particle field names to be parsed from org file keywords in the header.")

(defvar charge--site nil
  "The current charge site being emitted")

(defvar charge--route nil
  "The current charge route being emitted")

(defvar charge--particle nil
  "The current charge particle being emitted")

(defun charge-collect-org (files)
  (unless (listp files) (setq files '(files)))
  (mapcar
   (lambda (file)
     (with-temp-buffer
       (insert-file-contents file)
       (delay-mode-hooks (org-mode))
       (let ((particle (charge-particle (expand-file-name file)
                         :path file
                         :filename (file-name-nondirectory file))))
         (dolist (x (org-collect-keywords charge-org-keywords))
           (push (cons (intern (concat ":" (downcase (car x)))) (cadr x))
                 particle))
         (dolist (x (org-entry-properties 0))
           (push (cons (intern (concat ":" (downcase (car x)))) (cdr x))
                 particle))
         particle)))
   files))

(defun charge-collect-files (files)
  (unless (listp files) (setq files '(files)))
  (mapcar
   (lambda (file)
     (charge-particle (expand-file-name file)
       :path file
       :filename (file-name-nondirectory file)
       :extension (file-name-extension file)))
   files))

(defun charge-export-particle-org (particle)
  (let ((org-html-htmlize-output-type 'css))
    (save-window-excursion
      (with-temp-buffer
        (insert-file-contents (alist-get :id particle))
        (org-export-as 'charge nil nil t)))))

(defun charge-org-html-link (link desc _info)
  (let* ((path (org-element-property :path link))
         (href (if (and (bound-and-true-p charge--site) (bound-and-true-p charge--particle))
                   (charge-url
                    charge--site
                    (concat
                     (file-name-directory (alist-get :id charge--particle))
                     path))
                 path)))
    (format "<a href=\"%s\">%s</a>" href desc)))

(org-export-define-derived-backend 'charge 'html
  :translate-alist
  '((link . charge-org-html-link)))

(provide 'charge)