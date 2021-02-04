(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map)))
