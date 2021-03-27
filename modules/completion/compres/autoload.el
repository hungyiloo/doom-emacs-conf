;;; completion/compres/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +compres/minibuffer-default-add-function ()
          (autoload 'ffap-guesser "ffap")
          (with-selected-window (minibuffer-selected-window)
            (delete-dups
             (delq nil
                   (list (thing-at-point 'symbol)
                         (thing-at-point 'list)
                         (ffap-guesser)
                         (thing-at-point-url-at-point))))))
;;;###autoload
(defun +compres/consult-line-dwim ()
    "Conduct a text search on the current buffer.
If a selection is active, pre-fill the prompt with it."
    (interactive)
    (if (doom-region-active-p)
        (consult-line (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-line)))

;;;###autoload
(defun +compres/consult-line-symbol-at-point ()
    "Conduct a text search on the current buffer for the symbol at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +compres/consult-ripgrep-dwim (dir)
    "Conduct a text search on in the current (project) directory.
If a selection is active, pre-fill the prompt with it."
    (interactive "P")
    (if (doom-region-active-p)
        (consult-ripgrep dir (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
      (consult-ripgrep dir)))

;;;###autoload
(defun +compres/consult-ripgrep-other-project-dwim (dir)
    "Conduct a text search on in the current (project) directory.
If a selection is active, pre-fill the prompt with it."
    (interactive "P")
    (let ((default-directory
            (if-let (projects (projectile-relevant-known-projects))
                (completing-read "Search project: " projects nil t)
              (user-error "There are no known projects"))))
      (if (doom-region-active-p)
          (consult-ripgrep dir (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
        (consult-ripgrep dir))))

;;;###autoload
(defun +compres/consult-ripgrep-cwd ()
    (interactive)
    (consult-ripgrep default-directory))

;;;###autoload
(defun +compres/consult-ripgrep-other-cwd ()
    (interactive)
    (consult-ripgrep t))

;;;###autoload
(defun +compres/consult-ripgrep-notes ()
    (interactive)
    (unless (bound-and-true-p org-directory)
      (require 'org))
    (consult-ripgrep org-directory))

;;;###autoload
(defun +compres/consult-ripgrep-symbol-at-point (dir)
    "Conduct a text search on in the current (project) directory for the symbol at point."
    (interactive "P")
    (consult-ripgrep dir (thing-at-point 'symbol)))

;;;###autoload
(defun +compres/consult-ripgrep-notes-symbol-at-point (dir)
    "Conduct a text search on in the current (project) directory for the symbol at point."
    (interactive "P")
    (unless (bound-and-true-p org-directory)
      (require 'org))
    (consult-ripgrep org-directory (thing-at-point 'symbol)))

;;;###autoload
(defun +compres/consult-project-buffer ()
    (interactive)
    (when (doom-project-p)
      (run-at-time 0.1 nil #'execute-kbd-macro (kbd "p SPC"))) ; this feels DIRTY but it works
    (consult-buffer))

;;;###autoload
(defun +compres/consult-find-under-here (dir)
    (interactive "P")
    (consult-find (or dir default-directory)))

;;;###autoload
(defun +compres/consult-org-set-tags ()
      "Select tags to add to or remove from a headline.
  Choose one or more tags. Chosen tags that are already on the current headline will
  be removed. Chosen tags which are not, will be added."
      (interactive)
      (require 'org)
      (when (org-before-first-heading-p) (user-error "Not a headline."))
      (save-excursion
        (org-back-to-heading)
        (let* ((current (org-get-tags (point)))
               (selected (thread-last (org-get-buffer-tags)
                           (completing-read-multiple "Select org tag(s): "))))
          (org-set-tags
           (seq-uniq (append (seq-difference current selected)
                             (seq-difference selected current)))))))

;;;###autoload
(defun +compres/embark-export-prj-dired (files)
    "Create a dired buffer listing FILES in the current project root."
    (setq files (mapcar #'directory-file-name files))
    (when (dired-check-switches dired-listing-switches "A" "almost-all")
      (setq files (cl-remove-if-not
                   (lambda (path)
                     (let ((file (file-name-nondirectory path)))
                       (unless (or (string= file ".") (string= file ".."))
                         path)))
                   files)))
    (dired (cons (doom-project-root) files))
    (rename-buffer (format "*Embark Export Project Dired %s*" default-directory)))

;;;###autoload
(defun +compres/resolve-project-file (file)
    "Resolve a file using the project path as a prefix"
    (let* ((project-file (doom-path (doom-project-root) file))
           (target (or (and (file-exists-p project-file)
                            project-file)
                       file)))
      target))

;;;###autoload
(defun +compres/prj-find-file (file)
    (interactive "FFile:")
    (find-file (+compres/resolve-project-file file)))

;;;###autoload
(defun +compres/prj-find-file-other-window (file)
    (interactive "FFile:")
    (find-file-other-window (+compres/resolve-project-file file)))

;;;###autoload
(defun +compres/prj-delete-file (file)
    (interactive "FFile:")
    (delete-file (+compres/resolve-project-file file)))

;;;###autoload
(defun +compres/prj-rename-file (file newname)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Rename %s to file:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (rename-file (+compres/resolve-project-file file) newname))

;;;###autoload
(defun +compres/prj-copy-file (file newname)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Copy %s to file:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (copy-file (+compres/resolve-project-file file) newname))

;;;###autoload
(defun +compres/prj-ediff-files (file-a file-b)
    (interactive
     (let ((f (read-file-name "File:")))
       (list f
             (read-file-name (format "Compare %s to:" f)
                             (file-name-directory (+compres/resolve-project-file f))))))
    (ediff-files (+compres/resolve-project-file file-a) file-b))

;;;###autoload
(defun +compres/prj-embark-insert-relative-path (file)
    (interactive "FFile:")
    (embark-insert-relative-path (+compres/resolve-project-file file)))

;;;###autoload
(defun +compres/prj-embark-save-relative-path (file)
    (interactive "FFile:")
    (embark-save-relative-path (+compres/resolve-project-file file)))

;;;###autoload
(defun +compres/spell-correct ()
    "Correct spelling of word at point."
    (interactive)
    ;; spell-fu fails to initialize correctly if it can't find aspell or a similar
    ;; program. We want to signal the error, not tell the user that every word is
    ;; spelled correctly.
    (unless (;; This is what spell-fu uses to check for the aspell executable
             or (and ispell-really-aspell ispell-program-name)
             (executable-find "aspell"))
      (user-error "Aspell is required for spell checking"))

    (ispell-set-spellchecker-params)
    (save-current-buffer
      (ispell-accept-buffer-local-defs))
    (cl-destructuring-bind (start . end)
        (or (bounds-of-thing-at-point 'word)
            (user-error "No word at point"))
      (let ((word (thing-at-point 'word t))
            (orig-pt (point))
            poss ispell-filter)
        (ispell-send-string "%\n")
        (ispell-send-string (concat "^" word "\n"))
        (while (progn (accept-process-output ispell-process)
                      (not (string= "" (car ispell-filter)))))
        ;; Remove leading empty element
        (setq ispell-filter (cdr ispell-filter))
        ;; ispell process should return something after word is sent. Tag word as
        ;; valid (i.e., skip) otherwise
        (unless ispell-filter
          (setq ispell-filter '(*)))
        (when (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
        (cond
         ((or (eq poss t) (stringp poss))
          ;; don't correct word
          (message "%s is correct" (funcall ispell-format-word-function word))
          t)
         ((null poss)
          ;; ispell error
          (error "Ispell: error in Ispell process"))
         (t
          ;; The word is incorrect, we have to propose a replacement.
          (setq res (completing-read (format "Corrections for %S: " word) (nth 2 poss)))
          ;; Some interfaces actually eat 'C-g' so it's impossible to stop rapid
          ;; mode. So when interface returns nil we treat it as a stop.
          (unless res (setq res (cons 'break word)))
          (cond
           ((stringp res)
            (+compress--spell-correct res poss word orig-pt start end))
           ((let ((cmd (car res))
                  (wrd (cdr res)))
              (unless (or (eq cmd 'skip)
                          (eq cmd 'break)
                          (eq cmd 'stop))
                (+compress--spell-correct cmd poss wrd orig-pt start end)
                (unless (string-equal wrd word)
                  (+compress--spell-correct wrd poss word orig-pt start end))))))
          (ispell-pdict-save t))))))

(defun +compress--spell-correct (replace poss word orig-pt start end)
  (cond ((eq replace 'ignore)
         (goto-char orig-pt)
         nil)
        ((eq replace 'save)
         (goto-char orig-pt)
         (ispell-send-string (concat "*" word "\n"))
         (ispell-send-string "#\n")
         (setq ispell-pdict-modified-p '(t)))
        ((or (eq replace 'buffer) (eq replace 'session))
         (ispell-send-string (concat "@" word "\n"))
         (add-to-list 'ispell-buffer-session-localwords word)
         (or ispell-buffer-local-name ; session localwords might conflict
             (setq ispell-buffer-local-name (buffer-name)))
         (if (null ispell-pdict-modified-p)
             (setq ispell-pdict-modified-p
                   (list ispell-pdict-modified-p)))
         (goto-char orig-pt)
         (if (eq replace 'buffer)
             (ispell-add-per-file-word-list word)))
        (replace
         (let ((new-word (if (atom replace)
                             replace
                           (car replace)))
               (orig-pt (+ (- (length word) (- end start))
                           orig-pt)))
           (unless (equal new-word (car poss))
             (delete-region start end)
             (goto-char start)
             (insert new-word))))
        ((goto-char orig-pt)
         nil)))
