;;; lisp/hydras.el -*- lexical-binding: t; -*-

(defun my-mc-select-matches ()
  (interactive)
  (evil-mc-execute-for-all-cursors
   (lambda (args)
     (interactive)
     (when (thing-at-point-looking-at (caar evil-mc-pattern))
       (if (alist-get :real args)
           (progn
             (goto-char (match-beginning 0))
             (evil-visual-char)
             (goto-char (- (match-end 0) 1)))
         (setq region (evil-mc-create-region
                       (match-beginning 0)
                       (match-end 0)
                       'char)))))))

(defhydra my-mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors)
                       :post (progn
                               (evil-mc-resume-cursors)
                               (when evil-mc-pattern (my-mc-select-matches))))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _._: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   ^ ^                   _R_: remove all
_n_: skip & next   ^ ^                   _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ("Z" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" #'evil-mc-skip-and-goto-next-match)
  ("N" #'evil-mc-skip-and-goto-prev-match)
  ("J" #'evil-mc-make-cursor-move-next-line)
  ("K" #'evil-mc-make-cursor-move-prev-line)
  ("." #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

(defhydra my-sp-hydra (:color amaranth
                       :hint nil)
  "
^Navigation^           ^Editing^
^^^^----------------------------------------------------------------------------
_e_: forward sexp      _d_: kill sexp        _~_: transpose
_b_: backward sexp     _D_: kill whole line  _!_: raise
_w_: next sexp         _c_: comment          _+_: join
_W_: up sexp (forward) _<_: barf backward    _|_: split
_B_: sexp beginning    _>_: barf forward     _=_: clone
_E_: sexp end          _,_: slurp backward   _%_: convolute
_RET_: down sexp       _._: slurp forward    ___: splice
_v_: select sexp       _)_: wrap round       _<delete>_: splice kill forward
_V_: clear selection   _]_: wrap square      _<backspace>_: splice kill backward
_y_: yank selection    _}_: wrap curly       _p_: paste after
_Y_: yank sexp         ^ ^                   _P_: paste before

_u_: undo  _C-r_: redo  _C-SPC_: set mark _s_: toggle strict
"
  ("0" #'evil-beginning-of-line)
  ("$" #'evil-end-of-line)
  ("^" #'evil-first-non-blank)
  ("j" #'evil-next-line)
  ("k" #'evil-previous-line)
  ("h" #'evil-backward-char)
  ("l" #'evil-forward-char)
  ("y" #'evil-yank)
  ("Y" #'sp-copy-sexp)
  ("p" #'evil-paste-after)
  ("P" #'evil-paste-before)
  ("e" #'sp-forward-sexp)
  ("b" #'sp-backward-sexp)
  ("w" #'sp-next-sexp)
  ("B" #'sp-beginning-of-sexp)
  ("E" #'sp-end-of-sexp)
  ("W" #'sp-up-sexp)
  ("RET" #'sp-down-sexp)
  ("v" #'sp-mark-sexp)
  ("V" #'evil-exit-visual-state)
  ("o" #'exchange-point-and-mark)
  ("d" #'sp-kill-sexp)
  ("D" #'sp-kill-whole-line)
  ("c" #'evilnc-comment-operator)
  ("(" #'sp-wrap-round)
  (")" #'sp-wrap-round)
  ("[" #'sp-wrap-square)
  ("]" #'sp-wrap-square)
  ("{" #'sp-wrap-curly)
  ("}" #'sp-wrap-curly)
  ("<" #'sp-backward-barf-sexp)
  (">" #'sp-forward-barf-sexp)
  ("," #'sp-backward-slurp-sexp)
  ("." #'sp-forward-slurp-sexp)
  ("~" #'sp-transpose-sexp)
  ("!" #'sp-raise-sexp)
  ("+" #'sp-join-sexp)
  ("|" #'sp-split-sexp)
  ("=" #'sp-clone-sexp)
  ("%" #'sp-convolute-sexp)
  ("_" #'sp-splice-sexp)
  ("<delete>" #'sp-splice-sexp-killing-forward)
  ("<backspace>" #'sp-splice-sexp-killing-backward)
  ("z" #'evil-scroll-line-to-center)
  ("C-SPC" #'evil-visual-char)
  ("u" #'evil-undo)
  ("C-r" #'evil-redo)
  ("s" #'smartparens-strict-mode :exit t)
  ("q" nil "quit" :color blue)
  ("C-g" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))
