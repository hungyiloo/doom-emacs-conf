;;; config/git-gutter.el -*- lexical-binding: t; -*-

(after! git-gutter
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode org-mode))
  (defun my/org-hook-start-without-vc-gutter ()
    "Set up `git-gutter-mode' in the current buffer regardless of `git-gutter:disabled-modes' and leave it off initially."
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (when (or +vc-gutter-in-remote-files
                (not (file-remote-p (or file-name default-directory))))
        (if (null file-name)
            (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local)
          (when (vc-backend file-name)
            (if (and (display-graphic-p)
                     (require 'git-gutter-fringe nil t))
                (setq-local git-gutter:init-function      #'git-gutter-fr:init
                            git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                            git-gutter:clear-function     #'git-gutter-fr:clear
                            git-gutter:window-width -1)
              (setq-local git-gutter:init-function      'nil
                          git-gutter:view-diff-function #'git-gutter:view-diff-infos
                          git-gutter:clear-function     #'git-gutter:clear-diff-infos
                          git-gutter:window-width 1))
            (git-gutter-mode -1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))
  (add-hook! 'org-mode-hook #'my/org-hook-start-without-vc-gutter))
