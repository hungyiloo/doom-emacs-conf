;;; config/ansi-color.el -*- lexical-binding: t; -*-

(after! ansi-color
  (custom-set-faces!
    `(ansi-color-blue :foreground ,(doom-color 'blue) :background ,(doom-color 'blue))
    `(ansi-color-red :foreground ,(doom-color 'red) :background ,(doom-color 'red))
    `(ansi-color-cyan :foreground ,(doom-color 'cyan) :background ,(doom-color 'cyan))
    `(ansi-color-green :foreground ,(doom-color 'green) :background ,(doom-color 'green))
    `(ansi-color-yellow :foreground ,(doom-color 'yellow) :background ,(doom-color 'yellow))
    `(ansi-color-magenta :foreground ,(doom-color 'magenta) :background ,(doom-color 'magenta))
    `(ansi-color-bright-blue :foreground ,(doom-color 'blue 256) :background ,(doom-color 'blue 256))
    `(ansi-color-bright-red :foreground ,(doom-color 'red 256) :background ,(doom-color 'red 256))
    `(ansi-color-bright-cyan :foreground ,(doom-color 'cyan 256) :background ,(doom-color 'cyan 256))
    `(ansi-color-bright-green :foreground ,(doom-color 'green 256) :background ,(doom-color 'green 256))
    `(ansi-color-bright-yellow :foreground ,(doom-color 'yellow 256) :background ,(doom-color 'yellow 256))
    `(ansi-color-bright-magenta :foreground ,(doom-color 'magenta 256) :background ,(doom-color 'magenta 256))
    `(ansi-color-bright-black :foreground ,(doom-color 'base0 256) :background ,(doom-color 'base0 256))
    `(ansi-color-black :foreground ,(doom-color 'base0) :background ,(doom-color 'base0))
    `(ansi-color-white :foreground ,(doom-color 'fg) :background ,(doom-color 'fg))
    `(ansi-color-bright-white :foreground ,(doom-color 'fg 256) :background ,(doom-color 'fg 256))))
