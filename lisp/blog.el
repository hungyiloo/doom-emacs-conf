;;; lisp/blog.el -*- lexical-binding: t; -*-

(require 'org)
(require 'ox)
(require 'f)

(defun charge-render (template)
  (let (tag attr-name (content (list)) (attrs (list)))
    (dolist (x template)
      (cond ((and x (listp x))
             (push
              (charge-render x)
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

(defun charge--tag-is-void (tag)
  (when (memq tag '(area base br col embed hr img input link meta param source track wbr))
    t))

(defvar charge-node-keywords '("slug" "title" "date" "draft" "filetags")
  "The supported node field names to be parsed from org file keywords in the header.")

(defun charge-make-node-org (file)
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (let (node-alist)
      (dolist (x (org-collect-keywords charge-node-keywords))
        (push (cons (intern (concat ":" (downcase (car x)))) (cadr x))
              node-alist))
      (dolist (x (org-entry-properties 0))
        (push (cons (intern (concat ":" (downcase (car x)))) (cdr x))
              node-alist))
      (setf (alist-get :file node-alist) (expand-file-name file))
      node-alist)))

;; (alist-get :slug (charge-make-node-org "~/code/hungyi.net/content/posts/react-hook-use-debounce.org"))
;; (charge-make-node-org "~/Notes/roam/20210923142159-book_recommendations.org")

(defun charge-enrich-node-org-with-html (node)
  (let ((org-html-htmlize-output-type 'css))
    (save-window-excursion
      (with-temp-buffer
        (insert-file-contents (alist-get :file node))
        (org-export-to-buffer 'charge (buffer-name))
        (setf (alist-get :html node) (buffer-string))
        node))))

;; (charge-enrich-node-org-with-html (charge-make-node-org "~/Notes/roam/20210923142159-book_recommendations.org"))
;; (charge-enrich-node-org-with-html (charge-make-node-org "~/code/hungyi.net/content/posts/react-hook-use-debounce.org"))

(defun charge-site (&rest site)
  (let ((base-url (plist-get site :base-url))
        (output-dir (plist-get site :output-dir))
        (routes (plist-get site :routes)))
    (dolist (route routes)
      (let ((path (plist-get route :path))
            (nodes (plist-get route :nodes))
            (enricher (plist-get route :enricher))
            (renderer (plist-get route :renderer)))
        (dolist (node nodes)
          (message "exporting: %s" (alist-get :file node))
          (let ((enriched-node (funcall enricher node))
                (write-destination (f-join output-dir (funcall path node))))
            (mkdir (file-name-directory write-destination) t)
            (write-region
             (funcall renderer enriched-node route site)
             nil
             write-destination)))))))

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
     (let* ((option (and (string-match "::\\(.*\\)\\'" path)
                         (match-string 1 path)))
            (file-name (if (not option) path
                         (substring path 0 (match-beginning 0)))))
       (format "<a href=\"%s\">%s</a>" (expand-file-name file-name) desc))))

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

;; (charge-render '((ul
;;                   (li (button :class "apple" :disabled nil "shiny and red"))
;;                   (li (button :class "banana" :onClick "alert('foo')" "bent and yellow"))
;;                   (li (button :class "carrot" :disabled "" "pointy and orange")))
;;                  (p :class "prose" "lorem " (b :style "display: none;" "ipsum") " dolor sit amet")))
;; (charge-render `(div ,(+ 1 2 3)))
;; (charge-render `(section (div (img :src "kitten.jpg"))))
;; (charge-render `(ul ,(mapcar (lambda (x) `(li "Number: " ,x)) '(1 2 3))))
;; (charge-render `(html (head (title "My Blog")) (body "Hello World!")))

;; (defun my/blog-render-landing (title body)
;;   (charge-render `(html (head (title ,title)) (body ,body))))

;; (my/blog-render-landing "THE TITLE" "THE BODY")
