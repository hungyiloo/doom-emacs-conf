;;; config/restclient-mode.el -*- lexical-binding: t; -*-

(set-popup-rule!
    (format "\\(%s\\|%s\\)"
            restclient-same-buffer-response-name
            restclient-info-buffer-name)
    :height 30
    :quit t)
