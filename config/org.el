;;; config/org.el -*- lexical-binding: t; -*-

(after! org
  ;; Set custom header bullets
  (setq org-superstar-headline-bullets-list '("•"))
  (setq org-superstar-cycle-headline-bullets nil)
  ;; Show more whitespace in org mode when cycling
  (setq org-cycle-separator-lines -1)
  ;; Make sure org supports org-id stuff
  (add-to-list 'org-modules 'org-id)
  ;; Log CLOSED timestamp when notes are set to DONE state
  (setq org-log-done 'time)
  ;; Always use ID properties to store links
  (setq org-id-link-to-org-use-id 'use-existing)
  ;; Don't export org files with table of contents by default
  (setq org-export-with-toc nil)
  ;; Don't export section numbers
  (setq org-export-with-section-numbers nil)
  ;; Also show state changes (such as finished recurring tasks) in agenda
  (setq org-agenda-log-mode-items '(closed clock state))
  ;; Tweak priorities so unprioritized items are lowest
  (setq org-priority-start-cycle-with-default nil)
  (setq org-lowest-priority 67)
  (setq org-default-priority 68)
  ;; Tweak what shows up by default on opening org buffers
  (setq org-startup-folded 'content)
  ;; Don't inherit tags on sublevels
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-use-tag-inheritance nil)
  ;; Use relative file link paths always
  (setq org-link-file-path-type 'relative)

  ;; Allow large tables to be processed
  (setq org-table-convert-region-max-lines 9999)
  ;; When storing links by ID, add them to the normal `org-stored-links' variable
  (defadvice! +org--store-id-link-a (link)
    :filter-return #'org-id-store-link
    (when (and link org-store-link-plist)
      (add-to-list 'org-stored-links
                   (list (plist-get org-store-link-plist :link)
                         (plist-get org-store-link-plist :description))))
    link)
  ;; A function to copy the URL from an org mode link
  (defun my/org-retrieve-url-from-point ()
    "Copies the URL from an org link at the point"
    (interactive)
    (let ((plain-url (url-get-url-at-point)))
      (if plain-url
          (progn
            (kill-new plain-url)
            (message (concat "Copied: " plain-url)))
        (let* ((link-info (assoc :link (org-context)))
               (text (when link-info
                       ;; org-context seems to return nil if the current element
                       ;; starts at buffer-start or ends at buffer-end
                       (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                       (or (caddr link-info) (point-max))))))
          (if (not text)
              (error "Oops! Point isn't in an org link")
            (string-match org-link-bracket-re text)
            (let ((url (substring text (match-beginning 1) (match-end 1))))
              (kill-new url)
              (message (concat "Copied: " url))))))))

  (defun org-babel-execute:html (body _params)
    "Execute a block of HTML code.
This function is called by `org-babel-execute-src-block'."
    body)

  (defun org-babel-execute:tsx (body params)
    "Execute a block of TSX code.
This function is called by `org-babel-execute-src-block'.
Depends on esbuild being installed and available on the path"
    (with-temp-buffer
      (insert body)
      (call-process-region
       (point-min) (point-max)
       "esbuild"
       t t nil
       "--loader=tsx")
      (if (member "html" (split-string (alist-get :results params) " "))
          (concat "<script>\n" (buffer-string) "</script>")
        (buffer-string))))

  ;; Map `my/org-retrieve-url-from-point' to live with its org link friends
  (map! :map org-mode-map
        :localleader
        "O" #'consult-outline
        (:prefix ("l" . "links")
         "y" #'my/org-retrieve-url-from-point))

  ;; Some basic, experimental org mode presentation functionality
  ;; using narrowing and heading navigation
  (map! :map org-mode-map
        :nvi "C-S-SPC" (cmd! (widen) (goto-char 0))
        :nvi "C-^" (cmd! (let ((narrowed (buffer-narrowed-p)))
                           (when narrowed
                             (goto-char 0)
                             (widen))
                           (when (> (org-outline-level) 1)
                             (outline-up-heading 1)
                             (if narrowed
                                 (org-narrow-to-subtree)
                               (recenter 0)))))
        :nvi "C-S-v" (cmd! (org-narrow-to-subtree)
                           (goto-char 0))
        :nvi "C->" (cmd! (let ((narrowed (buffer-narrowed-p)))
                           (when narrowed
                             (goto-char 0)
                             (widen))
                           (org-forward-heading-same-level nil)
                           (if narrowed
                               (org-narrow-to-subtree)
                             (recenter 0))))
        :nvi "C-<" (cmd! (let ((narrowed (buffer-narrowed-p)))
                           (when narrowed
                             (goto-char 0)
                             (widen))
                           (org-backward-heading-same-level nil)
                           (if narrowed
                               (org-narrow-to-subtree)
                             (recenter 0)))))

  ;; Org agenda customization
  (setq org-agenda-span 3
        org-agenda-start-day "+0d"
        org-agenda-block-separator 32
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-delay-if-deadline t
        org-agenda-todo-ignore-timestamp t
        org-agenda-todo-ignore-with-date t
        org-agenda-breadcrumbs-separator "·")
  (require 'emojify)
  (setq org-agenda-category-icon-alist
        `(("Journal" ,(doom-path emojify-emojis-dir emojify-emoji-set "1f4d8.png") nil nil :ascent center :height ,(* 2(default-font-width)))
          ("Shopping" ,(doom-path emojify-emojis-dir emojify-emoji-set "1f6d2.png") nil nil :ascent center :height ,(* 2(default-font-width)))
          ("Recipes" ,(doom-path emojify-emojis-dir emojify-emoji-set "1f955.png") nil nil :ascent center :height ,(* 2(default-font-width)))
          ("" ,(doom-path emojify-emojis-dir emojify-emoji-set "25aa.png") nil nil :ascent center :height ,(* 2(default-font-width)))))
  (defun my/org-agenda-format-big-date (date)
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date 1 nil))
           (day (cadr date))
           (month (car date))
           (monthname (calendar-month-name month 1)))
      (format (if (org-agenda-today-p date)
                  "\n\n            ┏━━━━━━━━━━━━┓\n            ┃ %-2s %2d %s ┃\n            ┗━━━━━━━━━━━━┛"
                "\n\n             %-2s %2d %s\n             ‾‾‾‾‾‾‾‾‾‾")
              dayname day monthname)))
  (let ((my-agenda '((org-agenda-start-day "+0d")
                     (org-agenda-overriding-header "")
                     (org-agenda-repeating-timestamp-show-all nil)
                     (org-agenda-remove-tags t)
                     (org-agenda-prefix-format "%11t  %s")
                     (org-agenda-current-time-string "◁─────────── NOW")
                     (org-agenda-scheduled-leaders '("" "🔺 %sd ago: "))
                     (org-agenda-deadline-leaders '("‼ " "🔻 %sd: " "⚠ %sd ago: "))
                     (org-agenda-time-grid '((daily today remove-match)
                                             (900 1200 1500 1800)
                                             "" "┈┈┈┈┈┈┈┈┈┈┈┈"))
                     (org-agenda-format-date #'my/org-agenda-format-big-date)))
        (my-todo-list '((org-agenda-overriding-header "\n\n            ┏━━━━━━━━━━━━━━━┓\n            ┃ Hanging Todos ┃\n            ┗━━━━━━━━━━━━━━━┛")
                        (org-agenda-prefix-format "%10i  %-45b ")
                        (org-agenda-remove-tags t)
                        (org-agenda-format-date #'my/org-agenda-format-big-date))))
    (setq org-agenda-custom-commands
          `(("a" "My Agenda"
             ((agenda "" ,(append my-agenda
                                  '((org-agenda-span 7))))))
            ("t" "My Todo List"
             ((alltodo "" ,my-todo-list)))
            ("n" "My Agenda + Todo List"
             ((agenda "" ,(append my-agenda
                                  '((org-agenda-span 3))))
              (alltodo "" ,my-todo-list))))))

  ;; Redefine this command to fix agenda issues when going to buffers that
  ;; aren't yet "open"
  (defun org-agenda-goto (&optional highlight)
    "Go to the entry at point in the corresponding Org file."
    (interactive)
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; HACK: This `switch-to-buffer-other-window' doesn't work consisently in
      ;; not-yet-really-opened "links" in the agenda, so try to work around it.
      (unless buffer (user-error "Oops; you might have killed this buffer. Refresh the agenda to fix this link."))
      (other-window 1)
      (switch-to-buffer buffer)
      (widen)
      (push-mark)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
        (org-show-context 'agenda)
        (recenter (/ (window-height) 2))
        (org-back-to-heading t)
        (let ((case-fold-search nil))
          (when (re-search-forward org-complex-heading-regexp nil t)
            (goto-char (match-beginning 4)))))
      (run-hooks 'org-agenda-after-show-hook)
      (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

  ;; Fix issues with @tag fontification, according to my preference
  ;; Latest function can (probably) be found here:
  ;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes-ext-org.el
  (advice-add #'doom-themes-enable-org-fontification
              :around
              #'my/doom-themes-enable-org-fontification-replacement))

(after! org-roam
  (setq org-roam-verbose t)
  (custom-set-faces!
    `(org-roam-link :foreground ,(doom-color 'green) :inherit org-link)
    `(org-roam-link-curent :foreground ,(doom-color 'fg) :inherit org-link)
    `(org-roam-tag :foreground ,(doom-color 'base6) :weight unspecified))
  (map! :leader
        (:prefix-map ("n" . "notes")
         (:prefix ("r" . "roam")
          :desc "Migrate" "m" #'my/org-roam-create-note-from-headline)))
  (add-hook! 'org-roam-find-file-hook
    (defun my/org-roam-buffer-setup ()
      (auto-fill-mode 1))))
