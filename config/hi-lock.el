;;; config/hi-lock.el -*- lexical-binding: t; -*-

(after! hi-lock
  (custom-set-faces!
    `(hi-yellow :background ,(doom-blend (doom-color 'base1) "#FFFF00" 0.85) :foreground nil)
    `(hi-blue :background ,(doom-blend (doom-color 'base1) (doom-color 'blue 256) 0.7) :foreground nil)
    `(hi-pink :background ,(doom-blend (doom-color 'base1) (doom-color 'magenta 256) 0.8) :foreground nil)
    `(hi-green :background ,(doom-blend (doom-color 'base1) (doom-color 'green 256) 0.8) :foreground nil)
    `(hi-salmon :background ,(doom-blend (doom-color 'base1) (doom-color 'red 256) 0.8) :foreground nil)
    `(hi-aquamarine :background ,(doom-blend (doom-color 'base1) (doom-color 'teal 256) 0.7) :foreground nil)))
