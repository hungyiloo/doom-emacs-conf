;;; titlecase.el --- convert text to title case -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (let* ((case-fold-search nil)
         (str-length (length str))
         ;; A list of characters that indicate "word boundaries"
         ;; and are used to split the title into processable segments
         (word-boundary-chars '(?  ?– ?— ?- ?‑ ?/))
         ;; A list of characters that defers upcasing until the next character
         (prefixes-not-to-upcase '(?' ?\" ?\( ?\[ ?‘ ?“ ?’ ?” ?_))
         ;; A list of markers that indicate a "title within a title"
         ;; e.g. "The Lonely Reindeer: A Christmas Story"
         (new-phrase-markers '(?: ?. ?? ?\;))
         ;; A list of small words that should not be capitalized (in the right conditions)
         (small-words (split-string
                       "a an and as at but by en for if in of on or the to v v. vs vs. via"
                       " "))
         (fixed-all-caps-str (if (string-match-p "[a-z]" str)
                                 str
                               (downcase str)))
         ;; Reduce over a state machine to titlecase the string
         (final-state
          (cl-reduce (lambda (acc char)
                       (let* ((segment          (alist-get 'segment acc))
                              (result           (alist-get 'result acc))
                              (in-path-p        (alist-get 'in-path-p acc))
                              (first-word-p     (alist-get 'first-word-p acc))
                              (end-p            (eq (+ (length result) (length segment) 1) str-length))
                              (pop-p            (or end-p
                                                    (and (or (eq char ? ) (not in-path-p))
                                                         (member char word-boundary-chars))))
                              (segment-string   (apply #'string (reverse segment)))
                              (lowercase-rest-p (and (not end-p)
                                                     (not first-word-p)
                                                     (member (downcase segment-string) small-words)))
                              (pass-p           (or in-path-p
                                                    (string-match-p "\\w\\.\\w" segment-string)
                                                    (string-match-p "[A-Z]" segment-string)
                                                    (string-match-p "^https?:" segment-string)
                                                    (string-match-p "^[A-Za-z]:\\\\" segment-string)
                                                    (member ?@ segment))))
                         `((in-path-p .         ,(cond ((and (eq char ?/)
                                                             (or (not segment) (member (car segment) '(?. ?~))))
                                                        t)
                                                       ((eq char ? ) nil)
                                                       (t in-path-p)))
                           (first-word-p .      ,(if pop-p
                                                     (member (car segment) new-phrase-markers)
                                                   first-word-p))
                           (segment .           ,(unless pop-p (cons char segment)))
                           (result .            ,(if pop-p
                                                     ;; If we're ready to pop this segment, loop through it and do capilization if required.
                                                     (cl-reduce
                                                      (lambda (acc x)
                                                        (cons
                                                         (cond (pass-p x)                            ; skip this char coz we're skipping this segment
                                                               ((member x prefixes-not-to-upcase) x) ; start char of segment needs to be ignored
                                                               (lowercase-rest-p (downcase x))       ; already upcased start of segment, so lowercase the rest
                                                               (t (setq lowercase-rest-p t)          ; upcase the first char & flag that it's been done
                                                                  (upcase x)))
                                                         acc))
                                                      (reverse (cons char segment))
                                                      :initial-value result)
                                                   result)))))
                     fixed-all-caps-str
                     :initial-value
                     '((segment . nil)       ; current working segment
                       (result . nil)        ; result stack
                       (first-word-p . t)    ; is it the first word of a phrase?
                       (in-path-p . nil))))) ; are we inside of a filesystem path?
    ;; Reconstruct the string from the result of the exit state of the machine
    (apply #'string (reverse (alist-get 'result final-state)))))

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
;;               ("There are 100's of buyer's guides" "There Are 100's of Buyer's Guides")
;;               ("a trick perhaps? or not really." "A Trick Perhaps? Or Not Really.")
;;               ("this. is. good." "This. Is. Good.")
;;               ("some cats are fun; the others aren't" "Some Cats Are Fun; The Others Aren't"))
;;             )
;;     "\n")))

(provide 'titlecase)

