;;; config/emojify.el -*- lexical-binding: t; -*-

(use-package! emojify
  :commands (emojify-mode emojify-string)
  :init
  (after! hydra
    (advice-add #'hydra-show-hint
                :around
                (defun my/emojify-hydra-hint (orig-fun hint caller)
                  (funcall
                   orig-fun
                   (cons (car hint)
                         (cons (emojify-string (cadr hint))
                               (cddr hint)))
                   caller))))
  :config
  ;; I created a folder ~/.doom.d/.local/emojis/twemoji-latest and
  ;; downloaded the PNG assets from https://github.com/twitter/twemoji
  (setq emojify-download-emojis-p nil)
  (setq emojify-emoji-set "twemoji-latest")
  (setq emojify-emojis-dir (concat doom-private-dir ".local/emojis/"))
  ;; This file is generated via ~/.doom.d/generate-emoji-json.js
  ;; Run it using "node generate-emoji-json.js"
  (setq emojify-emoji-json (concat doom-private-dir "emoji.json"))

  (after! selectrum
    (advice-add #'selectrum--format-candidate
                :around
                (defun my/emojify-selectrum-candidate (orig-fun &rest args)
                  (emojify-string (apply orig-fun args)))))

  (after! consult
    ;; Fix emojify display issues after using consult.
    ;; `consult-line' and `consult-outline' in particular seems to mess with the buffer's
    ;; emojify region, so the following setup resets the global mode after use.
    (defun my/emojify-reset-global-mode (orig-fun &rest args)
      (condition-case nil
          (progn
            (apply orig-fun args)
            (global-emojify-mode -1)
            (global-emojify-mode +1))
        (quit (progn (global-emojify-mode -1)
                     (global-emojify-mode +1)))))
    (add-hook! 'consult-after-jump-hook
               #'emojify-redisplay-emojis-in-region)
    (advice-add #'consult-line :around #'my/emojify-reset-global-mode)
    (advice-add #'consult-outline :around #'my/emojify-reset-global-mode))

  ;; This is needed for fixing "regexp too big" issues in emojify
  (defun my/partition-list (list parts)
    "Partition LIST into PARTS parts.  They will all be the same length except
the last one which will be shorter. Doesn't deal with the case where there are
less than PARTS elements in LIST at all (it does something, but it may not be
sensible)."
    (cl-loop with size = (ceiling (length list) parts)
             and tail = list
             for part upfrom 1
             while tail
             collect (cl-loop for pt on tail
                              for i upfrom 0
                              while (< i size)
                              collect (car pt)
                              finally (setf tail pt))))
  (defun my/batch-list (list size)
    (my/partition-list
     list
     (ceiling (/ (length list) (float size)))))

  ;; Redefine this function to fix "regexp to big" issues
  (defun emojify-set-emoji-data ()
    "Read the emoji data for STYLES and set the regexp required to search them."
    (setq-default emojify-emojis (let ((json-array-type 'list)
                                       (json-object-type 'hash-table))
                                   (json-read-file emojify-emoji-json)))

    (let (unicode-emojis ascii-emojis)
      (ht-each (lambda (emoji data)
                 (when (string= (ht-get data "style") "unicode")
                   (push emoji unicode-emojis))

                 (when (string= (ht-get data "style") "ascii")
                   (push emoji ascii-emojis)))
               emojify-emojis)

      ;; Construct emojify-regexps such that github style are searched first
      ;; followed by unicode and then ascii emojis.
      ;;
      ;; NOTE: This has been modified by me, and I don't use :this: style of
      ;;       emoji or ascii emojis --- only unicode ones. I've also added
      ;;       batching because more than about 4200 emojis and regexp matching
      ;;       starts having issues.
      (setq emojify-regexps (mapcar
                             (lambda (unicode-emoji-batch) (regexp-opt unicode-emoji-batch))
                             (my/batch-list (sort unicode-emojis (lambda (a b) (> (length a) (length b)))) 4000))))

    (when emojify-user-emojis
      (if (emojify--verify-user-emojis emojify-user-emojis)
          ;; Create entries for user emojis
          (let ((emoji-pairs (mapcar (lambda (user-emoji)
                                       (cons (car user-emoji)
                                             (ht-from-alist (cdr user-emoji))))
                                     emojify-user-emojis)))
            (setq emojify--user-emojis (ht-from-alist emoji-pairs))
            (setq emojify--user-emojis-regexp (regexp-opt (mapcar #'car emoji-pairs))))
        (message "[emojify] User emojis are not in correct format ignoring them.")))

    (emojify-emojis-each (lambda (emoji data)
                           ;; Add the emoji text to data, this makes the values
                           ;; of the `emojify-emojis' standalone containing all
                           ;; data about the emoji
                           (ht-set! data "emoji" emoji)
                           (ht-set! data "custom" (and emojify--user-emojis
                                                       (ht-get emojify--user-emojis emoji)))))

    ;; Clear completion candidates cache
    (setq emojify--completing-candidates-cache nil))

  ;; Redefine this function to simplify the label on each emoji when inserting
  (defun emojify--get-completing-read-candidates ()
    "Get the candidates to be used for `emojify-completing-read'.

The candidates are calculated according to currently active
`emojify-emoji-styles' and cached"
    (let ((styles (mapcar #'symbol-name emojify-emoji-styles)))
      (unless (and emojify--completing-candidates-cache
                   (equal styles (car emojify--completing-candidates-cache)))
        (setq emojify--completing-candidates-cache
              (cons styles
                    (let ((emojis '()))
                      (emojify-emojis-each (lambda (key value)
                                             (when (seq-position styles (ht-get value "style"))
                                               (push (format "%s - %s"
                                                             key
                                                             (ht-get value "name"))
                                                     emojis))))
                      emojis))))
      (cdr emojify--completing-candidates-cache)))

  ;; Redefine this function to get rid of line spacing changes
  (defun emojify-completing-read (prompt &optional predicate require-match initial-input hist def inherit-input-method)
    "Read emoji from the user and return the selected emoji.

PROMPT is a string to prompt with, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, INHERIT-INPUT-METHOD correspond to the arguments for
`emojify-completing-read-function' and are passed to
‘emojify-completing-read-function’ without any interpretation.

For each possible emoji PREDICATE is called with a string of the form
'<emoji> - <name> (<style>)', the predicate should return nil if it the emoji should not be
displayed for selection.

For example the following can be used to display only github style emojis for
selection

\(emojify-completing-read \"Select a Github style emoji: \"
                         (lambda (display-string)
                           (s-suffix? display-string \"(github)\")))

This function sets up `selectrum', `ivy' and vanilla Emacs
completion UI to display properly emojis."
    (emojify-create-emojify-emojis)
    (let* ((emojify-minibuffer-reading-emojis-p t)
           (completion-ignore-case t)
           (candidates (emojify--get-completing-read-candidates))
           (minibuffer-setup-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                        minibuffer-setup-hook)))
      (car (split-string (funcall emojify-completing-read-function
                                  prompt
                                  candidates
                                  predicate
                                  require-match
                                  initial-input
                                  hist
                                  def
                                  inherit-input-method)
                         " ")))))
