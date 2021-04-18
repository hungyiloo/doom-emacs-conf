;;; config/restclient-mode.el -*- lexical-binding: t; -*-

(after! restclient-mode
  (set-popup-rule!
    (format "\\(%s\\|%s\\)"
            restclient-same-buffer-response-name
            restclient-info-buffer-name)
    :height 0.6
    :quit t))
