;;; recdesc.el --- Generator for recursive-descent parsers
;;
;; Copyright (C) 2000,2007 Stefan Bund

;; recdesc.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; recdesc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Der Syntax einer term deklaration ist:

;; <term-declaration> := (term <symbol> <arglist> <command>...)
;;
;; <command> := <directive>
;;            | (collect <directive-or-form>)
;;            | (return <form>)
;; 
;; <directive-or-form> := <directive> | <form>
;; 
;; <diretive> :=  (match <term> [<form>...])
;;             |  (or <command>...)
;;             |  (and <command>...)
;;             |  (maybe <command>...)
;;             |  (repeat <command>...)
;;             |  (commit)
;;             |  (fail)
;;             |  (progn <lisp-code>...)
;;
;; <term> := <symbol> | <form>

;; 
;;

;;; Change-Log:

;; $Log: recdesc.el,v $
;; Revision 1.3  2000/02/13 21:19:56  bund
;; Erste vollständige version von SqIV2
;;
;; Revision 1.2  2000/02/03 10:15:19  bund
;; *** empty log message ***
;;
;; Revision 1.1  2000/02/01 13:26:03  bund
;; *** empty log message ***
;;
;;

;;; Variables:

;;; Code:

(require 'cl)

(declaim (special position))
(declaim (special string))
(declaim (special commit-flag))
(declaim (special saved-position))
(declaim (special saved-elements))
(declaim (special elements))
(declaim (special return-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zunächst der Parser/Scanner zur deklaration von Termen

(defmacro recdesc-deferror (name message &rest classes)
  `(progn
     (put ',name 'error-conditions '(error sqi-error ,@classes ,name))
     (put ',name 'error-message ,message)))

(defmacro recdesc-declare (&rest forms)
  (cons 'progn
        (loop for form in forms
              collect (if (and (consp form)
                               (eq (car form) 'term)
                               (consp (cdr form))
                               (symbolp (cadr form))
                               (consp (cddr form))
                               (listp (caddr form)))
                          (recdesc-declare-term (cadr form)
                                                    (caddr form)
                                                    (cdddr form))
                        (signal 'recdesc-invalid-decl-component
                                (list form))))))

(recdesc-deferror recdesc-invalid-decl-component
  "Invalid component in recdesc-declare" recdesc-error)

(defun recdesc-declare-term (name arglist body)
  (let (return-form)
    `(defun ,(intern (concat "recdesc@@" (symbol-name name))) ,arglist
       (let (elements)
         (or (and ,(recdesc-parse-directive/and body)
		  ,(if return-form
		       return-form
		     '(or elements t))))))))

(defun recdesc-parse-command (command)
  (if (not (consp command))
      (signal 'recdesc-invalid-command
              (list command)))
  (cond ((eq (car command) 'return)
         (if (not (consp (cdr command)))
             (signal 'recdesc-invalid-command
                     (list command)))
         (setq return-form (cadr command))
         t)
        ((eq (car command) 'collect)
         (if (not (consp (cdr command)))
             (signal 'recdesc-invalid-command (list command)))
         `(recdesc-collect ,(recdesc-parse-directive-or-form (cadr command))))
        (t (recdesc-parse-directive command))))

(recdesc-deferror recdesc-invalid-command
  "Invalid scanner command" recdesc-error)

(defun recdesc-parse-directive-or-form (directive-or-form)
  (or (recdesc-parse-directive directive-or-form t)
      directive-or-form))

(defun recdesc-parse-directive (directive &optional nosignal)
  (if (not (consp directive))
      (if (not nosignal)
          (signal 'recdesc-invalid-directive
                  (list directive)))
    (cond ((eq (car directive) 'match)   (recdesc-parse-directive/match   (cdr directive)))
          ((eq (car directive) 'or)      (recdesc-parse-directive/or      (cdr directive)))
          ((eq (car directive) 'and)     (recdesc-parse-directive/and     (cdr directive)))
          ((eq (car directive) 'maybe)   (recdesc-parse-directive/maybe   (cdr directive)))
          ((eq (car directive) 'repeat)  (recdesc-parse-directive/repeat  (cdr directive)))
          ((eq (car directive) 'commit)  (recdesc-parse-directive/commit  (cdr directive)))
          ((eq (car directive) 'fail)    (recdesc-parse-directive/fail    (cdr directive)))
          ((eq (car directive) 'progn)   (recdesc-parse-directive/progn   (cdr directive)))
          (t (if (not nosignal)
                 (signal 'recdesc-invalid-directive
                         (list directive)))))))

(recdesc-deferror recdesc-invalid-directive
  "Invalid scanner directive" recdesc-error)

(defun recdesc-parse-directive/match (args)
  (if (not (consp args))
      (signal 'recdesc-invalid-match-arguments
              (list args)))
  (if (symbolp (car args))
      (cons (intern (concat "recdesc@@" (symbol-name (car args))))
            (cdr args))
    (if (not (null (cdr args)))
        (signal 'recdesc-invalid-match-arguments
                (list args)))
    (list 'recdesc-match-regexp (car args))))

(recdesc-deferror recdesc-invalid-match-arguments
  "Invalid arguments to match directive" recdesc-error)

(defun recdesc-parse-directive/or (args)
  (if (not (consp args))
      (signal 'recdesc-invalid-or-arguments
              (list args)))
  (cons 'or
        (loop for command in args
              collect (recdesc-parse-directive/and (list command)))))

(recdesc-deferror recdesc-invalid-or-arguments
  "Invalid arguments to or directive" recdesc-error)

(defun recdesc-parse-directive/and (args)
  (if (not (consp args))
      (signal 'recdesc-invalid-and-arguments
              (list args)))
  `(let ((saved-position position)
         (saved-elements elements)
         commit-flag)
     (or (and ,@(loop for command in args
                      collect (recdesc-parse-command command)))
         (recdesc-backtrack))))

(recdesc-deferror recdesc-invalid-and-arguments
  "Invalid arguments to and directive" recdesc-error)

(defun recdesc-parse-directive/maybe (args)
  (if (not (consp args))
      (signal 'recdesc-invalid-maybe-arguments
              (list args)))
  `(or ,(recdesc-parse-directive/and args) t))

(defun recdesc-parse-directive/repeat (args)
  `(loop for repeat-item = ,(recdesc-parse-directive/and args)
         while repeat-item
         collect repeat-item))

(defun recdesc-parse-directive/commit (args)
  (if args
      (signal 'recdesc-invalid-commit-arguments
              (list args)))
  `(setq commit-flag t))

(recdesc-deferror recdesc-invalid-commit-arguments
  "Invalid arguments to commit directive" recdesc-error)

(defun recdesc-parse-directive/fail (args)
  (if args
      (signal 'recdesc-invalid-fail-arguments
              (list args)))
  '(throw 'recdesc-failed nil))

(recdesc-deferror recdesc-invalid-fail-arguments
  "Invalid arguments to fail directive" recdesc-error)

(defun recdesc-parse-directive/progn (args)
  `(progn ,@args))

(put 'term 'lisp-indent-function 'defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Die folgenden Funktionen werden von den durch recdesc-declare
;; erzeugten termen benötigt

(defun recdesc-backtrack ()
  (progn
    (if commit-flag
	(throw 'recdesc-failed nil))
    (setq position saved-position
	  elements saved-elements)
    nil))

(defun recdesc-collect (value)
  (if value
      (progn
	(setq elements (nconc elements 
			      (list value)))
	value)))

(defun recdesc-match-regexp (regexp)
  (if (string-match (concat "^" regexp) (substring string position))
      (let ((start position))
	(setq position (+ position (match-end 0)))
	(substring string start position))))

(defun recdesc-match (term &rest args)
  (apply (intern (concat "recdesc@@" (symbol-name term))) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Toplevel calls to generated scanners

(defun recdesc-scan-string (term string &rest args)
  (let ((scanner (intern (concat "recdesc@@"
				 (symbol-name term))))
	(position 0)
	rv)
    (setq rv (catch 'recdesc-failed
	       (apply scanner args)))
    (if (not (string-match "^[ \t\n\r]*$" (substring string position)))
	nil
      rv)))

(provide 'recdesc)


;;; Local Variables:
;;; elisp-project-autoload-file-name: "sqi-autoload.el"
;;; End:
