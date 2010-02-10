;;; parasep.el --- more paragraph separators

;; Copyright 2007, 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: convenience
;; URL: http://user42.tuxfamily.org/parasep/index.html

;; parasep.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; parasep.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These few functions add further paragraph separators to
;; `paragraph-separate' and `paragraph-start'.  The functions are designed
;; to be used either from a mode hook or interactively for occasional
;; changes.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21 too.

;;; Install:

;; Put parasep.el in one of your `load-path' directories, and in your .emacs
;; add
;;
;;     (require 'parasep)
;;
;; Then give the functions a one-off try like `M-x parasep-dashes', or add
;; it to a hook
;;
;;     (add-hook 'text-mode-hook 'parasep-dashes)
;;
;; There's autoload cookies for the functions if you know how to use
;; `update-file-autoloads' and friends, after which add or customize the
;; hooks.

;;; History:

;; Version 1 - the first version

;;; Code:

(defun parasep-add (regexp)
  "An internal part of parasep.el.
Append REGEXP to `paragraph-separate' and `paragraph-start' if
it's not already present in those variables.

Checking already present means that repeating the parasep
commands doesn't make the variables ever longer.  The check only
looks for REGEXP as a substring of the variables, it doesn't
parse to see it's at the top-level.  As long as REGEXP is unique
enough that should be fine."

  (setq regexp (concat "\\|" regexp))
  (let ((check (concat (regexp-quote regexp)
                       "\\(\\'\\|\\\\|\\)")) ;; end of string or \|
        (case-fold-search nil))
    (unless (string-match check paragraph-separate)
      (set (make-local-variable 'paragraph-separate)
           (concat paragraph-separate regexp)))
    (unless (string-match check paragraph-start)
      (set (make-local-variable 'paragraph-start)
           (concat paragraph-start regexp)))))


;;-----------------------------------------------------------------------------

;;;###autoload
(defun parasep-dashes ()
  "Make a line of ---- dashes a paragraph separator.
`paragraph-start' and `paragraph-separate' are extended so that a
line of dashes a separator.

    -------------

Or a comment line of dashes similarly.

    ;; ----------

    /*---------*/

The pattern added is quite loose, simply an optional
`comment-start-skip' followed by 4 or more \"-\".  Perhaps in the
future this may have to be a bit tighter."

  (interactive)
  (parasep-add (concat
                (if (not (member comment-start-skip '(nil "")))
                    (concat "\\(\\(" comment-start-skip "\\)[ \t]*\\)?"))
                "----+[ \t]*")))

;; Don't think need to insist that the dashes run to the end-of-line (with
;; possible whitespace or comment-end).
;;
;; If asking this then cc-mode comment-end " */" is a bit strict and would
;; want to turn the space into [ \t] to allow tab as well as space.  (No
;; comment-end-skip regexp as of cc-mode 5.32.)
;;
;; ;; `comment-end-skip' not in xemacs21, but don't lock down the check at
;; ;; compile time, just in case someone fires up newcomment.el there
;; (or (and (boundp 'comment-end-skip) ;; not in xemacs21
;;          (not (member comment-end-skip '(nil "")))
;;          (concat "\\(\\(" comment-end-skip "\\)[ \t]*\\)?"))
;;     (and (not (member comment-end '(nil "")))
;;          (concat "\\(" (regexp-quote comment-end)
;;                  "[ \t]*\\)?")))
;; "$"

;;;###autoload
(defun parasep-empty-comments ()
  "Make an empty comment line a paragraph separator.
`paragraph-start' and `paragraph-separate' are extended so a
comment line with no text is a paragraph separator.

    ;; One paragraph.
    ;;                       <-- separator
    ;; Another paragraph.

This is good if you use `comment-empty-lines'.  The default
paragraph separators in Emacs are generally only geared towards
completely blank lines between comment paragraphs.

The commenting matched follows `comment-start-skip'.  If it's nil
or empty then there's no comment syntax and this function does
nothing."

  (interactive)
  (if (not (member comment-start-skip '(nil "")))
      (parasep-add (concat "\\(" comment-start-skip "\\)[ \t]*$"))))

;;;###autoload
(defun parasep-perl-pod ()
  "Make POD =foo directives a paragraph separator.
Usually this is unnecessary because there's a blank line between
directives and other text or other directives, but Perl-Gtk XS
crunched with Glib::CodeGen is instead like

    =for apidoc
    Something about this func.
    =cut

and treating directives as separators helps filling the text in
between."
  ;; an unindented =item, =for, etc, and standard directives are all lower
  ;; case
  (interactive)
  (parasep-add "=[a-z]"))


(provide 'parasep)

;;; parasep.el ends here
