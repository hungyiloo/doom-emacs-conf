;;; lisp/hydras.el -*- lexical-binding: t; -*-

(defun my/mc-select-matches ()
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

(defhydra my/mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors)
                       :post (progn
                               (evil-mc-resume-cursors)
                               (when evil-mc-pattern (my/mc-select-matches))))
  "
_z_: match all     _J_: make & go down   _._: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   _I_: make visual beg  _R_: remove all
_n_: skip & next   _A_: make visual end  _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern  "
  ("z" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" #'evil-mc-skip-and-goto-next-match)
  ("N" #'evil-mc-skip-and-goto-prev-match)
  ("J" #'evil-mc-make-cursor-move-next-line)
  ("K" #'evil-mc-make-cursor-move-prev-line)
  ("I" #'evil-mc-make-cursor-in-visual-selection-beg :color blue)
  ("A" #'evil-mc-make-cursor-in-visual-selection-end :color blue)
  ("." #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

(defhydra my/sp-hydra (:color amaranth
                       :hint nil
                       :pre (recenter)
                       :post (recenter))
  "
_e_: forward sexp     _RET_: down sexp        _x_: kill region      _,_: slurp backward ^^^^^^^^^^  _~_: transpose   ___: splice
_b_: backward sexp    _S-<return>_: up sexp   _d_: kill sexp        _._: slurp forward  ^^^^^^^^^^  _!_: raise       _<delete>_: splice kill forward
_w_: next sexp        _v_: select sexp        _D_: kill whole line  _<_: barf backward  ^^^^^^^^^^  _+_: join        _<backspace>_: splice kill backward
_E_: sexp end         _V_: clear selection    _c_: comment          _>_: barf forward   ^^^^^^^^^^  _|_: split
_B_: sexp beginning   _y_: yank selection     _p_: paste after      ^ ^                 ^^^^^^^^^^  _=_: clone
_RET_: down sexp      _Y_: yank sexp          _P_: paste before     _{__[__(__)__]__}_: wrap        _%_: convolute

_u_: undo  _C-r_: redo  _C-SPC_: set mark  _s_: toggle strict"
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
  ("S-<return>" #'sp-up-sexp)
  ("RET" #'sp-down-sexp)
  ("v" #'sp-mark-sexp)
  ("V" #'evil-exit-visual-state)
  ("o" #'exchange-point-and-mark)
  ("x" #'sp-kill-region)
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

(defhydra my/window-hydra (:color amaranth
                           :hint nil
                           :idle 1.0)
  "
_j_: ↓  _J_: ⬇  _]_: next window              _~_: swap                  _m_: maximize          _+_: increase height  _u_: undo
_k_: ↑  _K_: ⬆  _[_: previous window          _=_: balance               _S_: max horizontal    ___: decrease height
_l_: →  _L_: ➡  _}_: next win any frame       _s_: split horizontal      _V_: max vertical      _>_: increase width
_h_: ←  _H_: ⬅  _{_: previous win any frame   _v_: split vertical        _t_: transpose         _<_: decrease width
_T_: tear   ^^^  _d_: delete win               _D_: delete and kill       _r_: rotate downwards  _R_: rotate upwards   _F_: flip (vertical)  _f_: flop (horizontal)

_SPC_: ace window  _x_: ace delete  _M-w_: evil-window-map  _w_: quick next window  "
  ("SPC" #'ace-window :color blue)
  ("]" #'evil-window-next)
  ("[" #'evil-window-prev)
  ("}" #'next-window-any-frame)
  ("{" #'previous-window-any-frame)
  ("j" #'evil-window-down)
  ("k" #'evil-window-up)
  ("h" #'evil-window-left)
  ("l" #'evil-window-right)
  ("J" #'+evil/window-move-down)
  ("K" #'+evil/window-move-up)
  ("H" #'+evil/window-move-left)
  ("L" #'+evil/window-move-right)
  ("~" #'ace-swap-window)
  ("=" #'balance-windows)
  ("s" #'evil-window-split)
  ("v" #'evil-window-vsplit)
  ("t" #'transpose-frame)
  ("T" #'tear-off-window :color blue)
  ("+" #'evil-window-increase-height)
  ("_" #'evil-window-decrease-height)
  (">" #'evil-window-increase-width)
  ("<" #'evil-window-decrease-width)
  ("m" #'doom/window-maximize-buffer :color blue)
  ("S" #'doom/window-maximize-horizontally)
  ("V" #'doom/window-maximize-vertically)
  ("d" #'delete-window)
  ("x" #'ace-delete-window :color blue)
  ("D" #'kill-buffer-and-window :color blue)
  ("r" #'evil-window-rotate-downwards)
  ("R" #'evil-window-rotate-upwards)
  ("u" #'winner-undo)
  ("F" #'flip-frame)
  ("f" #'flop-frame)
  ("M-w" #'my/evil-window-map-launcher :color blue)
  ("w" #'evil-window-next :color blue)
  ("C-g" nil "quit" :color blue)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

(defun my/evil-window-map-launcher ()
  (interactive)
  (run-at-time 0 nil #'execute-kbd-macro (kbd "SPC W")))
