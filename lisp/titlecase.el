;;; titlecase.el --- convert text to title case -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (let* ((case-fold-search nil)
         (str-length (length str))
         ;; A list of markers that indicate start of a new phrase within the title, e.g. "The Lonely Reindeer: A Christmas Story"
         (new-phrase-chars '(?: ?. ?? ?\; ?\n ?\r)) ; must be followed by one of  word-boundary-chars
         (immediate-new-phrase-chars '(?\n ?\r))    ; immediately triggers new phrase behavior without waiting for word boundary
         ;; A list of characters that indicate "word boundaries"; used to split the title into processable segments
         (word-boundary-chars (append '(?  ?– ?— ?- ?‑ ?/) immediate-new-phrase-chars))
         ;; A list of small words that should not be capitalized (in the right conditions)
         (small-words (split-string "a an and as at but by en for if in of on or the to v v. vs vs. via" " "))
         ;; Fix if str is ALL CAPS
         (str (if (string-match-p "[a-z]" str) str (downcase str)))
         ;; Reduce over a state machine to do title casing
         (final-state (cl-reduce
                       (lambda (state char)
                         (let* ((result              (aref state 0))
                                (last-segment        (aref state 1))
                                (first-word-p        (aref state 2))
                                (in-path-p           (aref state 3))
                                (end-p               (eq (+ (length result) (length last-segment) 1)
                                                         str-length))                                     ; are we at the end of the input string?
                                (pop-p               (or end-p (and (or (eq char ? ) (not in-path-p))
                                                                    (member char word-boundary-chars))))  ; do we need to pop a segment onto the output result?
                                (segment             (cons char last-segment))                            ; add the current char to the current segment
                                (segment-string      (apply #'string (reverse segment)))                  ; the readable version of the segment
                                (last-segment-string (apply #'string (reverse last-segment)))             ; the readable version of the previous segment
                                (small-word-p        (member (downcase last-segment-string) small-words)) ; was the last segment a small word?
                                (capitalize-p        (or end-p first-word-p (not small-word-p)))          ; do we need to capitalized this segment or lowercase it?
                                (ignore-segment-p    (or in-path-p
                                                         (string-match-p "\\w\\.\\w" segment-string)      ; ignore hostnames and namespaces.like.this
                                                         (string-match-p "[A-Z]" segment-string)          ; ignore explicitly capitalized segments
                                                         (string-match-p "^https?:" segment-string)       ; ignore URLs
                                                         (string-match-p "^[A-Za-z]:\\\\" segment-string) ; ignore windows paths
                                                         (member ?@ segment))))                           ; ignore email addresses and user handles with @ symbol
                           (vector
                            ;; result
                            (if pop-p
                                (concat result (if ignore-segment-p
                                                   segment-string                                   ; put pop onto the result without processing
                                                 (titlecase--segment segment-string capitalize-p))) ; titlecase the segment before popping onto result
                              result)

                            ;; next segment
                            (unless pop-p segment)

                            ;; is first word
                            (if pop-p
                                (or (not last-segment)
                                    (member (car last-segment) new-phrase-chars)
                                    (member (car segment) immediate-new-phrase-chars))
                              first-word-p)

                            ;; in a path
                            (or (and (eq char ?/)
                                     (or (not last-segment) (member (car last-segment) '(?. ?~))))
                                (and (not (eq char ? )) in-path-p)))))
                       str
                       :initial-value
                       (vector nil      ; result stack
                               nil      ; current working segment
                               t        ; is it the first word of a phrase?
                               nil))))  ; are we inside of a filesystem path?
    (aref final-state 0)))

(defun titlecase--segment (segment capitalize-p)
  "Convert a title's inner SEGMENT to capitlized or lower case depending on CAPITALIZE-P, then return the result."
  (let* ((case-fold-search nil)
         (ignore-chars '(?' ?\" ?\( ?\[ ?‘ ?“ ?’ ?” ?_))
         (final-state (cl-reduce
                       (lambda (state char)
                         (let ((result (aref state 0))
                               (downcase-p (aref state 1)))
                           (cond
                            (downcase-p                 (vector (cons (downcase char) result) t))  ; already upcased start of segment, so lowercase the rest
                            ((member char ignore-chars) (vector (cons char result) downcase-p))    ; check if start char of segment needs to be ignored
                            (t                          (vector (cons (upcase char) result) t))))) ; haven't upcased yet, and we can, so do it
                       segment
                       :initial-value (vector nil (not capitalize-p)))))
    (thread-last (aref final-state 0)
      (reverse)
      (apply #'string))))

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
;;               ("drop. the. ball." "Drop. The. Ball.")
;;               ("some cats are fun; the others aren't" "Some Cats Are Fun; The Others Aren't")
;;               ("roses are red\nviolets are blue" "Roses Are Red\nViolets Are Blue")
;;               ("roses are red\nand violets are blue" "Roses Are Red\nAnd Violets Are Blue")))
;;     "\n")))

(provide 'titlecase)
