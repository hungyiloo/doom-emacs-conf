(use-package! emojify
  :commands (emojify-mode)
  :config
  ;; I created a folder ~/.doom.d/.local/emojis/twemoji-latest and
  ;; downloaded the PNG assets from https://github.com/twitter/twemoji
  (setq emojify-emoji-set "twemoji-latest")
  (setq emojify-emojis-dir (concat doom-private-dir ".local/emojis/"))
  ;; This file is generated via ~/.doom.d/generate-emoji-json.js
  ;; Run it using "node generate-emoji-json.js"
  (setq emojify-emoji-json (concat doom-private-dir "emoji.json"))

  (after! selectrum
    (defun emojify-ephemeral-buffer-p (buffer)
      "Determine if BUFFER is an ephemeral/temporary buffer."
      (and (not (minibufferp))
           (not (string-match-p selectrum--candidates-buffer (buffer-name buffer)))
           (string-match-p "^ " (buffer-name buffer))))

    (defun my-emojify-refresh-selectrum-candidates (&rest ignored)
      (with-current-buffer (get-buffer selectrum--candidates-buffer)
        (emojify-display-emojis-in-region 1 (buffer-size))))

    (advice-add #'selectrum--insert-candidates :after #'my-emojify-refresh-selectrum-candidates))

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

This function sets up `ido', `icicles', `helm', `ivy' and vanilla Emacs
completion UI to display properly emojis."
    (emojify-create-emojify-emojis)
    (let* ((emojify-minibuffer-reading-emojis-p t)
           (completion-ignore-case t)
           (candidates (emojify--get-completing-read-candidates))
           ;; Vanilla Emacs completion and Icicles use the completion list mode to display candidates
           ;; the following makes sure emojify is enabled in the completion list
           (completion-list-mode-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                            completion-list-mode-hook))
           ;; (Vertical) Ido and Ivy displays candidates in minibuffer this makes sure candidates are emojified
           ;; when Ido or Ivy are used
           (minibuffer-setup-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                        minibuffer-setup-hook))
           (helm-after-initialize-hook (cons #'emojify--completing-read-helm-hook
                                             (bound-and-true-p helm-after-initialize-hook))))
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
