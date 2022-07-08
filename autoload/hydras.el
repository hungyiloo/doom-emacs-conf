;;; autoload/hydras.el -*- lexical-binding: t; -*-

;;;###autoload
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

;;;###autoload (autoload #'my/mc-hydra/body "autoload/hydras" nil t)
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
_n_: skip & next   _A_: make visual end  _!_: pause/resume
_N_: skip & prev   ^ ^                   _p_: paste multiple

Current pattern: %s(replace-regexp-in-string \"%\" \"%%\" (or (caar evil-mc-pattern) \"\"))  "
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
  ("p" #'my/yank-rectangle-push-lines-after :color blue)
  ("P" #'my/yank-rectangle-push-lines :color blue)
  ("!" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

;;;###autoload (autoload #'my/sp-hydra/body "autoload/hydras" nil t)
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

;;;###autoload (autoload #'my/window-hydra/body "autoload/hydras" nil t)
(defhydra my/window-hydra (:color amaranth
                           :hint nil
                           :idle 1.0)
  "
_j_ ⬇️ _J_    _]_ ⏭️    _~_ 🔀     🪓 → _v_   🎈 _m_ → _S_   _<_ 🪟 _>_ ^^  _F_ 🔃    _d_ 🗡
_k_ ⬆️ _K_    _[_ ⏮    _=_ ☯️     ^↓^           ^↓^   ^ ^   ^ ^ ____+_     _f_ 🔁    _D_ 💥
_l_ ➡️ _L_    _}_ ⏩    _T_ 🪚     _s_           _V_   ^ ^   ^ ^ ^ ^^ ^     ^ ^       _u_ 💩 _C-r_
_h_ ⬅️ _H_    _{_ ⏪    _t_ 🪄     ^ ^           ^ ^   ^ ^

_SPC_ 🎯    _x_ 🔫    _M-w_ ⌨️    _w_ 👟    _|_ ♊    "
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
  ("S" #'doom/window-maximize-horizontally :color blue)
  ("V" #'doom/window-maximize-vertically :color blue)
  ("d" #'delete-window :color blue)
  ("x" #'ace-delete-window :color blue)
  ("D" #'kill-buffer-and-window :color blue)
  ;; ("r" #'evil-window-rotate-downwards) ; Don't actually understand what these do!
  ;; ("R" #'evil-window-rotate-upwards)
  ("u" #'winner-undo)
  ("C-r" #'winner-redo)
  ("F" #'flip-frame)
  ("f" #'flop-frame)
  ("M-w" #'my/evil-window-map-launcher :color blue)
  ("w" #'evil-window-next :color blue)
  ("|" #'my/split-vertically-with-last-buffer :color blue)
  ("C-g" nil "quit" :color blue)
  ("q" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))

;;;###autoload
(defun my/evil-window-map-launcher ()
  (interactive)
  (run-at-time 0 nil #'execute-kbd-macro (kbd "SPC W")))

;;;###autoload
(defun my/split-vertically-with-last-buffer ()
  (interactive)
  (evil-window-vsplit)
  (previous-buffer))

;;;###autoload (autoload #'my/tarzan-hydra/body "autoload/hydras" nil t)
(defhydra my/tarzan-hydra (:color amaranth
                           :hint nil
                           :pre (recenter)
                           :post (recenter))
  "
_w_: forward node    _e_: node end
_b_: node beginning
_j_: down node       _a_: expand region
_k_: up node         _i_: contract region   _v_: toggle mark
"
  ("w" #'tarzan-goto-next-sibling)
  ("b" #'tarzan-goto-prev-sibling-start)
  ("e" #'tarzan-goto-next-sibling-end)
  ("j" #'tarzan-goto-first-child)
  ("k" #'tarzan-goto-parent)
  ("a" #'tarzan-expand-region)
  ("i" #'tarzan-contract-region)
  ("v" #'evil-visual-char)
  ("q" nil "quit" :color blue)
  ("C-g" nil "quit" :color blue)
  ("<escape>" nil "quit" :color blue))
