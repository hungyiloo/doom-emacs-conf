;;; titlecase.el --- convert text to title case -*- lexical-binding: t; -*-

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (let ((case-fold-search nil)
        (segment nil)
        (result nil)
        (first-word-p t)
        (index 0)
        (word-boundary-chars '(?  ?- ?‑ ?/))
        (prefixes-not-to-upcase '(?' ?\" ?\( ?\[ ?‘ ?“ ?’ ?” ?_))
        (new-phrase-markers '(?:))
        (small-words (split-string
                      "a an and as at but by en for if in of on or the to v v. vs vs. via"
                      " "))
        (in-path-p nil))
    (mapc (lambda (char)
            (let* ((end-p (eq index (1- (length str))))
                   (pop-p (or end-p
                              (and (or (eq char ? ) (not in-path-p))
                                   (member char word-boundary-chars))))
                   (segment-string (apply #'string (reverse segment)))
                   (downcase-rest-p (and (not end-p)
                                         (not first-word-p)
                                         (member (downcase segment-string) small-words)))
                   (pass-p (or in-path-p
                               (string-match-p "\\w\\.\\w" segment-string)
                               (string-match-p "[A-Z]" segment-string)
                               (string-match-p "^https?:" segment-string)
                               (string-match-p "^[A-Za-z]:\\\\" segment-string)
                               (member ?@ segment))))
              (when pop-p
                (setq first-word-p (member (car segment) new-phrase-markers)))
              (setq segment (cons char segment))
              (when (eq char ? )
                (setq in-path-p nil))
              (when pop-p
                (when (and (eq char ?/)
                           (or (eq (length segment) 1)
                               (member (cadr segment) '(?. ?~))))
                  (setq in-path-p t))
                (setq
                 segment
                 (mapcar
                  (lambda (x)
                    (if (or pass-p
                            (member x prefixes-not-to-upcase))
                        x
                      (if downcase-rest-p
                          (downcase x)
                        (progn
                          (setq downcase-rest-p t)
                          (upcase x)))))
                  (reverse segment)))
                (setq result
                      (append result segment))
                (setq segment nil)))
            (setq index (1+ index)))
          (if (string-match-p "[a-z]" str)
              str
            (downcase str)))
    (apply #'string result)))

(defun titlecase-region (begin end)
  "Convert text in region from BEGIN to END to title case."
  (interactive "*r")
  (let ((pt (point)))
    (insert (titlecase-string (delete-and-extract-region begin end)))
    (goto-char pt)))

(defun titlecase-dwim ()
  "Convert the region or current line to title case.
If Transient Mark Mode is on and there is an active region, convert
the region to title case.  Otherwise, work on the current line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))

;; (defun titlecase-test ()
;;   (interactive)
;;   (message
;;    "\n%s\n"
;;    (string-join
;;     (mapcar (lambda (case)
;;               (let ((actual (titlecase-string (car case)))
;;                     (expected (cadr case)))
;;                 (format "%s %s | %s"
;;                         (if (string-equal expected actual)
;;                             "✅"
;;                           "❌")
;;                         expected
;;                         actual)))
;;             '(("the quick brown fox jumps over the lazy dog" "The Quick Brown Fox Jumps Over the Lazy Dog")
;;               ("'the great gatsby'" "'The Great Gatsby'")
;;               ("small word at the end is nothing to be afraid of" "Small Word at the End Is Nothing to Be Afraid Of")
;;               ("for step-by-step directions email someone@gmail.com" "For Step-by-Step Directions Email someone@gmail.com")
;;               ("2lmc spool: 'gruber on OmniFocus and vapo(u)rware" "2lmc Spool: 'Gruber on OmniFocus and Vapo(u)rware")
;;               ("Have you read “The Lottery”?" "Have You Read “The Lottery”?")
;;               ("Have you read “the lottery”?" "Have You Read “The Lottery”?")
;;               ("Have you read \"the lottery\"?" "Have You Read \"The Lottery\"?")
;;               ("your hair[cut] looks (nice)" "Your Hair[cut] Looks (Nice)")
;;               ("People probably won't put http://foo.com/bar/ in titles" "People Probably Won't Put http://foo.com/bar/ in Titles")
;;               ("Scott Moritz and TheStreet.com’s million iPhone la‑la land" "Scott Moritz and TheStreet.com’s Million iPhone La‑La Land")
;;               ("Scott Moritz and thestreet.com’s million iPhone la‑la land" "Scott Moritz and thestreet.com’s Million iPhone La‑La Land")
;;               ("BlackBerry vs. iPhone" "BlackBerry vs. iPhone")
;;               ("Notes and observations regarding Apple’s announcements from ‘The Beat Goes On’ special event" "Notes and Observations Regarding Apple’s Announcements From ‘The Beat Goes On’ Special Event")
;;               ("Read markdown_rules.txt to find out how _underscores around words_ will be interpretted" "Read markdown_rules.txt to Find Out How _Underscores Around Words_ Will Be Interpretted")
;;               ("Q&A with Steve Jobs: 'That's what happens in technology'" "Q&A With Steve Jobs: 'That's What Happens in Technology'")
;;               ("What is AT&T's problem?" "What Is AT&T's Problem?")
;;               ("Apple deal with AT&T falls through" "Apple Deal With AT&T Falls Through")
;;               ("this v that" "This v That")
;;               ("this vs that" "This vs That")
;;               ("this v. that" "This v. That")
;;               ("this vs. that" "This vs. That")
;;               ("The SEC's Apple probe: what you need to know" "The SEC's Apple Probe: What You Need to Know")
;;               ("'by the way, small word at the start but within quotes.'" "'By the Way, Small Word at the Start but Within Quotes.'")
;;               ("Starting sub-phrase with a small word: a trick, perhaps?" "Starting Sub-Phrase With a Small Word: A Trick, Perhaps?")
;;               ("Sub-phrase with a small word in quotes: 'a trick, perhaps?'" "Sub-Phrase With a Small Word in Quotes: 'A Trick, Perhaps?'")
;;               ("Sub-phrase with a small word in quotes: \"a trick, perhaps?\"" "Sub-Phrase With a Small Word in Quotes: \"A Trick, Perhaps?\"")
;;               ("\"Nothing to Be Afraid of?\"" "\"Nothing to Be Afraid Of?\"")
;;               ("a thing" "A Thing")
;;               ("Dr. Strangelove (or: how I Learned to Stop Worrying and Love the Bomb)" "Dr. Strangelove (Or: How I Learned to Stop Worrying and Love the Bomb)")
;;               ("  this is trimming" "  This Is Trimming")
;;               ("IF IT’S ALL CAPS, FIX IT" "If It’s All Caps, Fix It")
;;               ("___if emphasized, keep that way___" "___If Emphasized, Keep That Way___")
;;               ("What could/should be done about slashes?" "What Could/Should Be Done About Slashes?")
;;               ("Never touch paths like /var/run before/after /boot" "Never Touch Paths Like /var/run Before/After /boot")
;;               ("What about relative paths like ./profile and ~/downloads/music?" "What About Relative Paths Like ./profile and ~/downloads/music?")
;;               ("And windows paths like c:\\temp\\scratch too" "And Windows Paths Like c:\\temp\\scratch Too")
;;               ("There are 100's of buyer's guides" "There Are 100's of Buyer's Guides"))
;;             )
;;     "\n")))

(provide 'titlecase)

