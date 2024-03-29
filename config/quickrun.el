;;; config/quickrun.el -*- lexical-binding: t; -*-

;; (use-package! quickrun
;;   :init
;;   (map! :leader
;;         (:prefix-map ("c" . "code")
;;          :desc "Replace region quickrun" "Q" #'my/replace-region-quickrun
;;          :desc "Eval region quickrun" "q" #'my/eval-region-quickrun
;;          :desc "Eval region elisp" "e"  #'my/eval-region-elisp
;;          :desc "Replace region elisp" "E" #'my/replace-region-elisp))

;;   :commands (quickrun
;;              quickrun-region
;;              quickrun-replace-region
;;              my/eval-region-elisp
;;              my/eval-region-quickrun
;;              my/replace-region-elisp
;;              my/replace-region-quickrun)

;;   :config
;;   ;; doom's advice for `quickrun--outputter-replace-region' always gave me
;;   ;; nil errors for region boundaries, so I've disabled it and written my
;;   ;; own functions below. See `my/replace-region-quickrun' and `my/replace-region-elisp'
;;   ;; (advice-remove #'quickrun--outputter-replace-region #'+eval--quickrun-fix-evil-visual-region-a)

;;   (evil-define-operator my/replace-region-elisp (beg end)
;;     "Evaluate selection as elisp and replace it with its result.

;; Often even if I'm not in emacs-lisp-mode, I still want to evaluate
;; short expressions as elisp quickly, without having to deal with the quirks
;; of quickrun. This function lets me do it quickly."
;;     :move-point nil
;;     (interactive "<r>")
;;     (kill-region beg end)
;;     (condition-case nil
;;         (prin1 (eval (read (current-kill 0)))
;;                (current-buffer))
;;       (error (message "Invalid expression")
;;              (insert (current-kill 0)))))

;;   (evil-define-operator my/eval-region-elisp (beg end)
;;     "Evaluate selection as elisp and replace it with its result.

;; Often even if I'm not in emacs-lisp-mode, I still want to evaluate
;; short expressions as elisp quickly, without having to deal with the quirks
;; of quickrun. This function lets me do it quickly."
;;     :move-point nil
;;     (interactive "<r>")
;;     (+emacs-lisp-eval beg end))

;;   (evil-define-operator my/replace-region-quickrun (beg end)
;;     "Evaluate selection using quickrun and replace it with its result.

;; This seems to work better for me than doom's provided `+eval:replace-region'"
;;     :move-point nil
;;     (interactive "<r>")
;;     (if (eq major-mode 'emacs-lisp-mode)
;;         (+eval/region-and-replace beg end)
;;       (progn
;;         ;; Setting the mark manually using evil's provided region beg/end
;;         ;; allows quickrun to work as if evil didn't exist
;;         (set-mark beg)
;;         (goto-char end)
;;         (activate-mark)
;;         ;; Now the mark is set and activated, hand off to quickrun
;;         (quickrun-replace-region beg end))))

;;   (evil-define-operator my/eval-region-quickrun (beg end)
;;     "Evaluate selection using quickrun and replace it with its result.

;; This seems to work better for me than doom's provided `+eval:replace-region'"
;;     :move-point nil
;;     (interactive "<r>")
;;     (if (eq major-mode 'emacs-lisp-mode)
;;         (+eval:region beg end)
;;       (progn
;;         ;; Setting the mark manually using evil's provided region beg/end
;;         ;; allows quickrun to work as if evil didn't exist
;;         (set-mark beg)
;;         (goto-char end)
;;         ;; Now the mark is set and activated, hand off to quickrun
;;         (setq quickrun-option-outputter nil) ; have to reset this for some reason?!
;;         (quickrun-region beg end))))

;;   (defun quickrun--outputter-replace-region ()
;;     "Replace region with quickrun output, and truncate the last character if it's a newline"
;;     (let ((output (buffer-substring-no-properties
;;                    (point-min)
;;                    (let* ((eob (point-max))
;;                           (one-before-eob (- eob 1))
;;                           (last-char (when (>= one-before-eob 0)
;;                                        (buffer-substring-no-properties one-before-eob eob)))
;;                           (last-char-newline-p (string-equal "\n" last-char)))
;;                      (if last-char-newline-p
;;                          one-before-eob
;;                        eob)))))
;;       (with-current-buffer quickrun--original-buffer
;;         (delete-region (region-beginning) (region-end))
;;         (insert (ansi-color-filter-apply output))
;;         (setq quickrun-option-outputter quickrun--original-outputter))))

;;   ;; Fix "selecting deleted buffer" errors for doom's quickrun overlay smarts
;;   (advice-add
;;    #'+eval-display-results-in-overlay
;;    :around
;;    (defun my/eval-display-results-in-overlay-killed-buffer-safe (orig-fun output &optional source-buffer)
;;      "Don't apply any overlays if SOURCE-BUFFER is provided but it's already killed."
;;      (when (or (not source-buffer)
;;                (buffer-live-p source-buffer))
;;        (funcall orig-fun output source-buffer))))

;;   (advice-add
;;    #'+eval-display-results
;;    :around
;;    (defun my/eval-display-results-strip-ansi-before (orig-fun output &optional source-buffer)
;;      (funcall orig-fun
;;               (ansi-color-filter-apply output)
;;               source-buffer)))

;;   ;; Fix typescript quickrunning by simplfiying using deno
;;   ;; NOTE: Deno must be externally installed within the environment for this to work
;;   (add-to-list 'quickrun--language-alist
;;                '("deno"
;;                  (:command . "deno run")
;;                  (:exec . ("cp %s %n.ts" "%c --quiet %n.ts"))
;;                  (:remove . ("%n.ts"))
;;                  (:description . "Run Typescript file with deno")))
;;   (add-to-list 'quickrun--major-mode-alist
;;                '(tsx-mode . "deno"))
;;   (add-to-list 'quickrun--major-mode-alist
;;                '(typescript-mode . "deno")))
