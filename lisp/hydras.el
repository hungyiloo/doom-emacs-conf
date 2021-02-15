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
_z_: match all     _J_: make & go down   _._: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   _I_: make visual beg  _R_: remove all
_n_: skip & next   _A_: make visual end  _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ("z" #'evil-mc-make-all-cursors :color blue)
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

(defhydra my-sp-hydra (:color amaranth
                       :hint nil)
  "
^Navigation^               ^Editing^
^^^^-----------------------------------------------------------------------------------
_e_: forward sexp          _x_: kill region       _~_: transpose
_b_: backward sexp         _d_: kill sexp         _!_: raise
_w_: next sexp             _D_: kill whole line   _+_: join
_E_: sexp end              _c_: comment           _|_: split
_B_: sexp beginning        _<_: barf backward     _=_: clone
_RET_: down sexp           _>_: barf forward      _%_: convolute
_S-<return>_: up sexp      _,_: slurp backward    ___: splice
_v_: select sexp           _._: slurp forward     _<delete>_: splice kill forward
_V_: clear selection       _)_: wrap round        _<backspace>_: splice kill backward
_y_: yank selection        _]_: wrap square       _p_: paste after
_Y_: yank sexp             _}_: wrap curly        _P_: paste before

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

(defhydra my-window-hydra (:color amaranth
                           :hint nil)
  "
_w_: ace window       _~_: swap              _m_: maximize
_j_: down             _=_: balance           _S_: maximize horizontal
_k_: up               _s_: split horizontal  _V_: maximize vertical
_l_: right            _v_: split vertical    _d_: delete window
_h_: left             _T_: tear off          _D_: quit and kill buffer
_J_: move down        _+_: increase height   _]_: next window
_k_: move up          ___: decrease height   _[_: previous window
_l_: move right       _>_: increase width    _}_: next window any frame
_h_: move left        _<_: decrease width    _{_: previous window any frame
_u_: undo             _r_: rotate downwards  _R_: rotate upwards
_t_: transpose        _F_: flip (vertical)   _f_: flop (horizontal)

_M-w_: evil-window-map  _SPC_: quick next window

"
  ("w" #'ace-window)
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
  ("T" #'tear-off-window)
  ("+" #'evil-window-increase-height)
  ("_" #'evil-window-decrease-height)
  (">" #'evil-window-increase-width)
  ("<" #'evil-window-decrease-width)
  ("m" #'doom/window-maximize-buffer)
  ("S" #'doom/window-maximize-horizontally)
  ("V" #'doom/window-maximize-vertically)
  ("d" #'delete-window)
  ("D" #'kill-buffer-and-window)
  ("r" #'evil-window-rotate-downwards)
  ("R" #'evil-window-rotate-upwards)
  ("u" #'winner-undo)
  ("F" #'flip-frame)
  ("f" #'flop-frame)
  ("M-w" #'my-evil-window-map-launcher :color blue)
  ("SPC" #'evil-window-next :color blue)
  ("C-g" nil "quit" :color blue)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

(defun my-evil-window-map-launcher ()
  (interactive)
  (run-at-time 0.1 nil #'execute-kbd-macro (kbd "SPC W")))
