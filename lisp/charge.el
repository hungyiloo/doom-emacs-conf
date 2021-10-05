;;; lisp/charge.el -*- lexical-binding: t; -*-

(require 'org)
(require 'ox)
(require 'f)
(require 'seq)

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
      (setq charge--particle nil)
      (setq charge--route nil)
      (setq charge--site nil))))

(defun charge-route (particles &rest options)
  (declare (indent defun))
  (let ((route-alist (mapcar (lambda (x) (apply #'cons x))
                             (seq-partition options 2))))
    (setf (alist-get :particles route-alist) particles)
    route-alist))

(defun charge-particle (&rest data)
  (mapcar (lambda (x) (apply #'cons x))
          (seq-partition data 2)))

(defun charge-url (site particle-or-id)
  (let ((urls (alist-get :urls site)))
    (if (listp particle-or-id)
        (gethash (alist-get :id particle-or-id) urls)
      (gethash particle-or-id urls))))

(defun charge-html (template)
  (let (tag attr-name (content (list)) (attrs (list)))
    (dolist (x template)
      (cond ((and x (listp x))
             (push
              (charge-html x)
              content))
            ((and (not tag) x (symbolp x)) (setq tag x))
            ((keywordp x) (setq attr-name x))
            (attr-name (push (cons attr-name x) attrs)
                       (setq attr-name nil))
            (t (push (format "%s" x) content))))
    (let ((tag-is-void (charge--tag-is-void tag)))
      (concat
       (when (eq tag 'html)
         "<!DOCTYPE html>\n")
       (when tag
         (format
          (if tag-is-void "<%s%s/>" "<%s%s>")
          tag
          (apply #'concat
                 (mapcar
                  (lambda (attr)
                    (format
                     (if (cdr attr) " %s=\"%s\"" " %s")
                     (substring (symbol-name (car attr)) 1) (cdr attr)))
                  (nreverse attrs)))))
       (unless tag-is-void (apply #'concat (nreverse content)))
       (when (and tag (not tag-is-void))
         (format "</%s>" tag))))))

(defun charge-write (text path)
  (write-region text nil path))

(defun charge-format (format-string key)
  (lambda (particle)
    (format format-string (alist-get key particle))))

(defun charge--tag-is-void (tag)
  (when (memq tag '(area base br col embed hr img input link meta param source track wbr))
    t))

(defcustom charge-org-keywords '("slug" "title" "date" "draft" "filetags")
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
       (let ((particle (charge-particle
                        :id (expand-file-name file)
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
     (charge-particle
      :path file
      :id (expand-file-name file)
      :filename (file-name-nondirectory file)
      :extension (file-name-extension file)))
   files))

;; (alist-get :slug (charge-make-particle-org "~/code/hungyi.net/content/posts/react-hook-use-debounce.org"))
;; (charge-make-particle-org "~/Notes/roam/20210923142159-book_recommendations.org")

(defun charge-export-particle-org (particle)
  (let ((org-html-htmlize-output-type 'css))
    (save-window-excursion
      (with-temp-buffer
        (insert-file-contents (alist-get :id particle))
        (org-export-to-buffer 'charge (buffer-name))
        (buffer-string)))))

;; (charge-enrich-particle-org (charge-make-particle-org "~/Notes/roam/20210923142159-book_recommendations.org"))
;; (charge-enrich-particle-org (charge-make-particle-org "~/code/hungyi.net/content/posts/react-hook-use-debounce.org"))

(with-eval-after-load 'org
  (org-link-set-parameters
   "charge"
   :complete
   #'charge-link-complete-file
   :follow
   #'org-link-open-as-file
   :face
   (lambda (path)
     (if
         (or
          (file-remote-p path)
          (and IS-WINDOWS
               (string-prefix-p "\\\\" path))
          (file-exists-p path))
         'org-link
       '(warning org-link)))
   :export
   (lambda (path desc _backend)
     (message "%s %s" charge--site charge--particle)
     (format
      "<a href=\"%s\">%s</a>"
      (if (and (bound-and-true-p charge--site) (bound-and-true-p charge--particle))
          (charge-url
           charge--site
           (concat
            (file-name-directory (alist-get :id charge--particle))
            path))
        path)
      desc)))

  (org-export-define-derived-backend 'charge 'html
    :translate-alist
    '(;; Replace the HTML generation code to prevent ox-html from adding
      ;; headers and stuff around the HTML generated for the `body` tag.
      (template . (lambda (contents _i) contents))
      ;; Opinionated setting for generating headline anchor links with
      ;; the headline text and to strip away the auto-generated IDs
      ;; (headline . (lambda (headline contents info)
      ;;               ;; Don't override existing value, so users can still put
      ;;               ;; whatever they want
      ;;               (unless (org-element-property :CUSTOM_ID headline)
      ;;                 (let ((headline-slug (weblorg--slugify (org-element-property :raw-value headline))))
      ;;                   (org-element-put-property headline :CUSTOM_ID headline-slug)))
      ;;               ;; Replace these IDs that don't have much use. That
      ;;               ;; will cause way less noise when re-generating a project without
      ;;               ;; any changes.
      ;;               (replace-regexp-in-string
      ;;                " id=\".+-org[0-9a-f]\\{7\\}\""
      ;;                ""
      ;;                (org-html-headline headline contents info))))
      )))

(defun charge-link-complete-file (&optional arg)
  "Create a file link using completion."
  (concat "charge:"
          (file-relative-name (read-file-name "File: "))))

;; (charge-html '((ul
;;                   (li (button :class "apple" :disabled nil "shiny and red"))
;;                   (li (button :class "banana" :onClick "alert('foo')" "bent and yellow"))
;;                   (li (button :class "carrot" :disabled "" "pointy and orange")))
;;                  (p :class "prose" "lorem " (b :style "display: none;" "ipsum") " dolor sit amet")))
;; (charge-html `(div ,(+ 1 2 3)))
;; (charge-html `(section (div (img :src "kitten.jpg"))))
;; (charge-html `(ul ,(mapcar (lambda (x) `(li "Number: " ,x)) '(1 2 3))))
;; (charge-html `(html (head (title "My Blog")) (body "Hello World!")))

;; (defun my/blog-render-landing (title body)
;;   (charge-html `(html (head (title ,title)) (body ,body))))

;; (my/blog-render-landing "THE TITLE" "THE BODY")

(provide 'charge)
