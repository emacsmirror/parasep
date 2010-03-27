;;; parasep.el --- more paragraph separators

;; Copyright 2007, 2008, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 3
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

;; Designed for Emacs 21 and up, works in XEmacs 21, believe works in Emacs
;; 20.

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
;; Version 2 - new parasep-texinfo-@*
;;           - better check for parts already in regexp
;; Version 3 - correction to custom-add-option

;;; Code:

(defun parasep-regexp-valid-p (str)
  "Return non-nil if string STR is a valid regexp."
  (condition-case nil
      (progn (string-match str "") t)
    (invalid-regexp nil)))

(defun parasep-regexp-split (regexp)
  "Return a list of the toplevel alternates of REGEXP.
REGEXP is split on top-level \\| so for instance
\"a\\|\\(b\\|c\\)\" gives a list (\"a\" \"\\(b\\|c\\)\").

If REGEXP is invalid then currently there's no error but there's
no splitting past the bad point."

  (let* ((ret  (split-string regexp "\\\\|"))
         (upto ret))
    (while (cdr upto)
      (if (parasep-regexp-valid-p (car upto))
          (setq upto (cdr upto))
        (setcar upto (concat (car upto) "\\|" (cadr upto)))
        (setcdr upto (cddr upto))))
    ret))
(put 'parasep-regexp-split 'pure t)

(defun parasep-add-to-regexp-var (var regexp)
  "Add REGEXP to variable VAR if not already present.
VAR is a symbol, the name of a variable containing a regexp
string.  If REGEXP is not already among the toplevel alternates
in VAR then it's prepended.

The return is the new value of VAR."

  (let ((old (symbol-value var)))
    (if (member regexp (parasep-regexp-split old))
        old
      (set var (concat regexp "\\|" old)))))

(defun parasep-add (regexp)
  "An internal part of parasep.el.
Append REGEXP to `paragraph-separate' and `paragraph-start' if
it's not already present in those variables.

Checking for already present means that repeating the parasep
commands doesn't make the variables ever longer."

  (parasep-add-to-regexp-var (make-local-variable 'paragraph-separate) regexp)
  (parasep-add-to-regexp-var (make-local-variable 'paragraph-start)    regexp))


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

;;;###autoload
(defun parasep-texinfo-@* ()
  "Make a line with \"@*\" start or separate a paragraph.
An @* alone on a line is a paragraph separator, an @* at the
beginning of a non-empty line is the start of a paragraph.

The effect is that paragraph filling won't flow an @* into
surrounding lines, so the texinfo source keeps a line break
similar to what @* will produce in the formatted output.

\(As of Emacs 23.1, the default settings have all alphabetical
@foo commands as paragraph separators, but not @*.)"

  (interactive)
  (parasep-add-to-regexp-var (make-local-variable 'paragraph-start)
                             "@\\*\\(\\s-\\|$\\)")
  (parasep-add-to-regexp-var (make-local-variable 'paragraph-separate)
                             "@\\*\\s-*$"))

;;;###autoload
(custom-add-option 'texinfo-mode-hook 'parasep-texinfo-@*)


(provide 'parasep)

;;; parasep.el ends here
