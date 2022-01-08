;;; config/consult.el -*- lexical-binding: t; -*-

(defvar my/consult-initial-narrow-config
  '((my/consult-project-buffer . ?p)))

(use-package! consult
  :config
  ;; Adjust some keybindings to use consult equivalents
  (map! :leader
        ;; "," #'my/consult-project-buffer
        ;; "<" #'consult-buffer
        (:prefix-map ("s" . "search")
         "f" #'+vertico/consult-fd)
        (:prefix-map ("M" . "mode")
         "M" #'consult-mode-command
         "N" #'consult-minor-mode-menu)
        (:prefix-map ("i" . "insert")
         "c" #'my/consult-color
         "C" #'my/consult-palette))

  ;; Add initial narrowing hook
  (defun my/consult-initial-narrow-setup ()
    (when-let (key (alist-get this-command my/consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'my/consult-initial-narrow-setup)

  ;; Make occur mode (with consult) act more similarly
  ;; to wgrep mode
  (map! :map occur-mode-map
        :n "C-x C-p" nil
        :n "C-c C-p" #'occur-edit-mode)
  (map! :map occur-edit-mode-map
        :n "C-x C-q" nil
        :n "C-c C-k" #'occur-cease-edit
        :n "C-c C-c" #'occur-cease-edit
        :n "Z Z" #'occur-cease-edit
        :n "Z Q" #'occur-cease-edit)

  ;; Some convenience mappings for narrowing
  (map! :map narrow-map
        "l" #'consult-focus-lines
        "w" #'my/widen-dwim)

  ;; REVIEW: Using doom's default previewing behavior for now, but apparent we
  ;; can customize `consult--read-config' to change this behavior more easily.
  ;; (setq consult-preview-key (kbd "C-."))
  ;; (setq consult-config '((consult-line :preview-key any)))

  (after! evil
    ;; Integrate with evil jumping
    (evil-set-command-property #'consult--jump :jump t)

    ;; Integrate with evil searching
    (advice-add #'consult-line
                :after
                (defun my/consult-line-evil-search-integrate (&rest _args)
                  (when-let ((str (substring-no-properties (car consult--line-history))))
                    (unless (eq str (car evil-ex-search-history))
                      (add-to-history 'evil-ex-search-history str)
                      (setq evil-ex-search-pattern (list str nil t))
                      (setq evil-ex-search-direction 'forward)
                      (when evil-ex-search-persistent-highlight
                        (evil-ex-search-activate-highlight evil-ex-search-pattern))))))))

(use-package! consult-flycheck
  :commands consult-flycheck
  :init (map!
         :map flycheck-command-map
         "z" #'consult-flycheck))

(defun my/consult-project-buffer ()
  (interactive)
  ;; See my/consult-initial-narrow-config
  (let ((this-command (if (doom-project-p)
                          this-command
                        #'consult-buffer)))
    (consult-buffer)))
