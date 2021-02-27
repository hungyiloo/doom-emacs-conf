;;; config/git-gutter.el -*- lexical-binding: t; -*-

(use-package! git-gutter
  :defer t
  :init
  ;; HACK: `+vc-gutter-init-maybe-h' to support "quiet start" of git-gutter,
  ;; it does all that's required to initialized git-gutter but does not switch
  ;; on the minor mode by default.
  ;;
  ;; This is mainly for my org-mode usage where there's heavy use of visual-line
  ;; mode and olivetti mode that seem to interact badly with git-gutter to make
  ;; up and down line movement unpredictable
  (setq git-gutter:quiet-start-modes '(org-mode))
  (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (when (not (memq major-mode git-gutter:disabled-modes))
        (let ((file-name (buffer-file-name (buffer-base-buffer))))
          (cond
           ((and (file-remote-p (or file-name default-directory))
                 (not +vc-gutter-in-remote-files)))
           ;; If not a valid file, wait until it is written/saved to activate
           ;; git-gutter.
           ((not (and file-name (vc-backend file-name)))
            (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
           ;; Allow git-gutter or git-gutter-fringe to activate based on the type
           ;; of frame we're in. This allows git-gutter to work for silly geese
           ;; who open both tty and gui frames from the daemon.
           ((if (and (display-graphic-p)
                     (require 'git-gutter-fringe nil t))
                (setq-local git-gutter:init-function      #'git-gutter-fr:init
                            git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                            git-gutter:clear-function     #'git-gutter-fr:clear
                            git-gutter:window-width -1)
              (setq-local git-gutter:init-function      'nil
                          git-gutter:view-diff-function #'git-gutter:view-diff-infos
                          git-gutter:clear-function     #'git-gutter:clear-diff-infos
                          git-gutter:window-width 1))
            (when (not (memq major-mode git-gutter:quiet-start-modes))
              (git-gutter-mode +1))
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))
