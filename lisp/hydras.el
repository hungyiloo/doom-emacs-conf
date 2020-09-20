;;; lisp/hydras.el -*- lexical-binding: t; -*-

(defhydra my-mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _z_: toggle here
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
  ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" #'evil-mc-resume-cursors "quit" :color blue)
  ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))

(defhydra my-sp-hydra (:color amaranth
                       :hint nil)
  "
^Navigation^           ^Editing^
^^^^----------------------------------------------------------------------------
_l_: next sexp         _X_: kill hybrid      _T_: transpose hybrid
_h_: backward sexp     _x_: kill sexp        _t_: transpose
_L_: forward sexp      _<_: barf backward    _r_: raise
_H_: previous sexp     _>_: barf forward     _/_: split
_k_: up sexp (back)    _,_: slurp backward   _|_: splice
_j_: down sexp         _._: slurp forward    _=_: clone
_K_: down sexp (back)  _?_: slurp hybrid     _<delete>_: splice kill forward
_J_: up sexp           _d_: kill whole line  _<backspace>_: splice kill backward
_b_: sexp beginning    _c_: comment
_e_: sexp end

_u_: undo  _C-r_: redo

_s_: toggle strict
"
  ("l" #'sp-next-sexp)
  ("h" #'sp-backward-sexp)
  ("L" #'sp-forward-sexp)
  ("H" #'sp-previous-sexp)
  ("b" #'sp-beginning-of-sexp)
  ("e" #'sp-end-of-sexp)
  ("k" #'sp-backward-up-sexp)
  ("j" #'sp-down-sexp)
  ("J" #'sp-up-sexp)
  ("K" #'sp-backward-down-sexp)
  ("X" #'sp-kill-hybrid-sexp)
  ("x" #'sp-kill-sexp)
  ("d" #'sp-kill-whole-line)
  ("c" #'sp-comment)
  ("<" #'sp-backward-barf-sexp)
  (">" #'sp-forward-barf-sexp)
  ("," #'sp-backward-slurp-sexp)
  ("." #'sp-forward-slurp-sexp)
  ("?" #'sp-slurp-hybrid-sexp)
  ("t" #'sp-transpose-sexp)
  ("T" #'sp-transpose-hybrid-sexp)
  ("r" #'sp-raise-sexp)
  ("/" #'sp-split-sexp)
  ("|" #'sp-splice-sexp)
  ("<delete>" #'sp-splice-sexp-killing-forward)
  ("<backspace>" #'sp-splice-sexp-killing-backward)
  ("=" #'sp-clone-sexp)
  ("s" #'smartparens-strict-mode :exit t)
  ("u" #'undo-fu-only-undo)
  ("C-r" #'undo-fu-only-redo)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))
