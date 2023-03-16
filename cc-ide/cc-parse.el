;;; cc-parse.el --- Generator for recursive-descent parsers
;;
;; Copyright (C) 2007 Stefan Bund

;; cc-parse.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; cc-parse.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; This is a very rudimentary parser to parse a single C++ argument
;; declaration. The goal of this parser is, given an argument
;; declaration find the name of the argument and the position of that
;; name within the declaration.
;;
;; The current version does not support string- or charachter literals
;; and comments within an argument declaration.

;;;; Code:

(require 'recdesc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-parse-arg (arg)
  ;; Returns a cons ( POSITION . NAME ) where POSITION is the position
  ;; of the argument in ARG and NAME is the name of that
  ;; argument. Will return 'nil, if the argument name is omitted in
  ;; the declaration.
  (recdesc-scan-string 'decl arg))

(recdesc-declare

 (term optional-whitespace ()
   (match "[ \t\n\r]*"))

 (term word ()
   (match optional-whitespace)
   (collect (match "\\w+\\b"))
   (return (cons (- position (length (car elements)))
                 (car elements))))

 (term symbol ()
   (match optional-whitespace)
   (match word)
   (maybe (repeat (match optional-whitespace)
                  (match "::")
                  (match optional-whitespace)
                  (match word)))
   (match optional-whitespace)
   (maybe (match "<")
          (match tokens)
          (match optional-whitespace)
          (match ">")))

 (term tokens ()
   (match optional-whitespace)
   (or (and (match "(") (commit) (match tokens) (match ")"))
       (and (match "\\[") (commit) (match tokens) (match "\\]"))
       (and (match "{") (commit) (match tokens) (match "}"))
       (and (match "<") (commit) (match tokens) (match ">"))
       (match "[^][(){}<>]*")))

 (term decl ()
   (match optional-whitespace)
   (collect (or (match primary)
                (match group)
                (match member-pointer)
                (match modifier)
                (match word)))
   (maybe (match arglist))
   (maybe (repeat (match array)))
   (match optional-whitespace)
   (return (car elements)))

 (term primary ()
   (match optional-whitespace)
   (match symbol)
   (match optional-whitespace)
   (collect (match decl))
   (return (car elements)))

 (term group ()
   (match optional-whitespace)
   (match "(")
   (collect (match decl))
   (match optional-whitespace)
   (match ")")
   (return (car elements)))

 (term member-pointer ()
   (match optional-whitespace)
   (match symbol)
   (match optional-whitespace)
   (match "::")
   (match optional-whitespace)
   (match "*")
   (commit)
   (match decl))

 (term modifier ()
   (match optional-whitespace)
   (match "const\\|volatile\\|\\*\\|&")
   (commit)
   (collect (match decl))
   (return (car elements)))

 (term array ()
   (match optional-whitespace)
   (match "\\[")
   (commit)
   (match tokens)
   (match optional-whitespace)
   (match "\\]")
   (return (car elements)))

 (term arglist ()
   (match optional-whitespace)
   (match "(")
   (commit)
   (match tokens)
   (match optional-whitespace)
   (match ")")))

;; To debug a term, place the name of the term's function into 'fn',
;; place the curor at the end of the line and presse C-x C-e. You may
;; then use 'edebug-defun' on the declaration produced by this.
;;
;; (let ((fn 'recdesc@@decl)) (save-excursion (insert (prin1-to-string (symbol-function fn)))) (insert "\n\n") (forward-char 1) (delete-char 6) (insert "defun " (symbol-name fn)) (forward-sexp) (insert "\n") (let ((start (point))) (forward-line 1) (replace-string ")" ")\n" nil start (point)) (indent-region (save-excursion (goto-char start) (forward-line -1) (point)) (point) nil)))

(provide 'cc-parse)
