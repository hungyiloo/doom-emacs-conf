;;; titlecase.el --- convert text to title case -*- lexical-binding: t; -*-

;;; Copyright (C) 2013 Jason R. Blevins <jrblevin@sdf.org>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Version: 1.1
;;; Author: Jason R. Blevins <jrblevin@sdf.org>
;;; Keywords: title case, capitalization, writing.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Currently, this is simply a wrapper for the `titlecase` Perl script
;; written by John Gruber and Aristotle Pagaltzis.  This script can be
;; found at <https://github.com/ap/titlecase>.  If `titlecase` is not
;; in your path, you must change the value of `titlecase-command'
;; accordingly.

;;; Code:

(defvar titlecase-command "titlecase")

(defconst titlecase-buffer "*titlecase output*")

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (with-temp-buffer
    (insert str)
    (call-process-region (point-min) (point-max) titlecase-command t t nil)
    ;; Skip trailing newline omitted by titlecase
    (buffer-substring (point-min) (1- (point-max)))))

(defun titlecase-string-new (str)
  "Convert string STR to title case and return the resulting string."
  (let ((case-fold-search nil)
        (segment nil)
        (result nil)
        (index 0)
        (first-word-p t)
        (word-boundary-chars '(? ?-))
        (prefixes-not-to-upcase '(?' ?\" ?\( ?\[ ?‘ ?“))
        (small-words (split-string
                      "a an and as at but by en for if in of on or the to v v. vs vs. via"
                      " ")))
    (mapc (lambda (char)
            (let* ((end-p (eq index (1- (length str))))
                   (pop-p (or end-p
                              (member char word-boundary-chars)))
                   (downcase-p (and (not end-p)
                                    (not first-word-p)
                                    (member (downcase (apply #'string segment)) small-words)))
                   (pass-p (or (string-match-p "[A-Z]" (apply #'string segment))
                               (member ?@ segment))))
              (setq segment (append segment (list char)))
              (when pop-p
                (setq
                 segment
                 (mapcar
                  (lambda (x)
                    (if (or pass-p
                            (member x prefixes-not-to-upcase))
                        x
                      (if downcase-p
                          (downcase x)
                        (progn
                          (setq downcase-p t)
                          (upcase x)))))
                  segment))
                (setq result
                      (append result segment))
                (setq segment nil)
                (setq first-word-p nil)))
            (setq index (1+ index)))
          str)
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

(defun titlecase-test ()
  (interactive)
  (dolist (case '(("the quick brown fox jumps over the lazy dog" "The Quick Brown Fox Jumps Over the Lazy Dog")
                  ("'the great gatsby'" "'The Great Gatsby'")
                  ("small word at the end is nothing to be afraid of" "Small Word at the End Is Nothing to Be Afraid Of")
                  ("for step-by-step directions email someone@gmail.com" "For Step-by-Step Directions Email someone@gmail.com")
                  ("2lmc spool: 'gruber on OmniFocus and vapo(u)rware" "2lmc Spool: 'Gruber on OmniFocus and Vapo(u)rware")))
    (let ((actual (titlecase-string-new (car case)))
          (expected (cadr case)))
      (message "%s | %s %s"
               expected
               actual
               (if (string-equal expected actual)
                   "✅"
                 "❌")))))

(provide 'titlecase)

;;; titlecase.el ends here
