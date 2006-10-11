;;; varcmd.el --- Flexible command handling
;;
;; $Id: varcmd.el,v 1.14 2000/02/26 10:20:47 bund Exp $
;;
;; Copyright (C) 1998 Stefan Bund

;; varcmd.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; varcmd.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;;; Change-Log:

;; $Log: varcmd.el,v $
;; Revision 1.14  2000/02/26 10:20:47  bund
;; Support für separator und undefine aktualisiert
;;
;; Revision 1.13  2000/02/13 21:17:49  bund
;; define-key-last implementiert und verwendet
;; vcmd-define-menu implementiert
;;
;; Revision 1.12  2000/01/26 15:32:08  bund
;; call-interactively anstelle von command-execute verwenden
;; backquote in vcmd-bind-menu
;;
;; Revision 1.11  2000/01/08 16:13:23  bund
;; vcmd-flag-handler implementiert
;;
;; Revision 1.10  1999/11/22 14:30:18  bund
;; Weitere XEmacs anpassungen
;;
;; Revision 1.9  1999/08/03 07:55:33  bund
;; erste (alpha) anpassung an xemacs
;;
;; Revision 1.8  1999/05/07 11:40:31  bund
;; noforms implementiert
;;
;; Revision 1.7  1998/09/03 11:25:29  bund
;; added 'expression vcmd type
;;
;; Revision 1.6  1998/07/11 19:17:53  bund
;; BUGFIX: seperator->separator :-)
;;
;; Revision 1.5  1998/07/06 09:08:37  bund
;; added 'separator to vcmd-bind-menu
;;
;; Revision 1.4  1998/07/03 08:56:16  bund
;; add menu-bar items added by vcmd-define-entry to menu-bar-final-items
;;
;; Revision 1.3  1998/06/26 15:41:29  bund
;; BUGFIX: nil-command interpreted correctly
;;
;; Revision 1.2  1998/06/22 08:55:34  bund
;; new semantics for handlers: Call with the args to the handler symbol
;; as arguments, not with a list of the arguments
;; modulized vcmd-define-key
;;
;; Revision 1.1  1998/06/19 10:44:50  bund
;; added varcmd.el
;;
;;

;;; Variables:

(defvar vcmd-handlers 
  '((function . vcmd-call-function)
    (lambda . vcmd-call-lambda-expression)
    (macro . cvmd-call-kbd-macro)
    (value . vcmd-return-value)
    (expression . vcmd-expression))
  "Alist of handlers for different command types")

(defvar vcmd-flag-handlers nil
  "Alist of flag handlers for differnt flags")

(defvar vcmd-command-symbol 0
  "Sequence number for generation of unique command names")

;;; Code:

(defun vcmd (command &optional value interactive)
  "Call COMMAND as en extended command as defined in vcmd-handlers.

If COMMAND is a cons cell and the car of COMMAND can be found in vcmd-handlers,
the COMMAND is executed by passing the cdr of COMMAND to the handler found in 
vcmd-handlers. vcmd then returns the return value of the handler.

If no handler is found, and optional VALUE is non-nil, the return value of 
vcmd is COMMAND. 

If VALUE is not given or is nil, then COMMAND should be an executable entry, 
e.g. a symbol or an array (keyboard macro).

If INTERACTIVE is non-nil, then the function is called interactively. If 
a handler is used to execute the command, the handler is passed t as second arg."
  (if (consp command)
      (let ((handler (cdr (assq (car command) vcmd-handlers))))
	(if handler
	    (apply handler interactive (cdr command))
	  command))
    (if value
	command
      (if (arrayp command)
	  (execute-kbd-macro command)
	(if interactive
	    (if (commandp command)
		(call-interactively command)
	      (funcall command))
	  (funcall command))))))
	      
(defun vcmd-call-function (interactive fn &rest args)
  "Call (car ARG) or ARG as function, suplying (cdr ARG) as arguments.
If ARG is a consp then if INTERACTIVE is non-nil, (cdr ARG) is non-nil
and (car ARG) is a command, call function with command-execute otherwise
use apply.

If ARG is not a cons cell, call ARG with command-execute, if it is a
command, otherwise use funcall."
  (if (and interactive
	   (null args)
	   (commandp fn))
      (command-execute fn)
    (apply fn args)))

(defun vcmd-call-lambda-expression (interactive &rest body)
  "call ARG as a lambda expression (without leading lambda).
If INTERACTIVE is non-nil and (cons 'lambda ARG) is a command, use
command-execute, otherwise use funcall."
  (if (and interactive
	   (commandp (cons 'lambda body)))
      (command-execute (cons 'lambda body))
    (funcall (cons 'lambda body))))

(defun vcmd-call-kbd-macro (interactive macro)
  "call ARG as keyboard macro"
  (execute-kbd-macro macro))

(defun vcmd-return-value (interactive value)
  "return ARG as return value"
  value)

(defun vcmd-expression (interactive &rest expression)
  (eval (cons 'progn expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vcmd-encapsulate-fn (definition forms)
  "Encapsulate call of DEFINITION using vcmd and return the
lambda expression."
  (let ((flag (and (consp definition) 
		   (assq (car definition) vcmd-flag-handlers))))
    (if flag
	(funcall (cdr flag) (cdr definition) forms)
      (append forms
	      (list (list 'vcmd
			  (list 'quote
				definition)
			  nil
			  t))))))

(defun vcmd-get-symbol (definition &rest forms)
  "Fetch a new symbol and set it's function definition to a call of 
DEFINITION. If FORMS is non-nil, theese forms are executed prior to 
calling DEFINITION. If DEFINITION is nil, an unbound symbol is returned."
  (setq vcmd-command-symbol (1+ vcmd-command-symbol))
  (let ((sym (intern (concat "vcmd-cmd-"
			     (number-to-string vcmd-command-symbol)))))
    (fset sym `(lambda ()
		 (interactive)
		 (let ((current-prefix-arg current-prefix-arg))
		   ,@(vcmd-encapsulate-fn definition forms))))
    sym))

(defun define-key-last (keymap key def)
  "Like define-key, mat make KEY the last entry in KEYMAP instead of
the first. KEY must contain just one event."
  (let ((last-key (loop for def in (reverse keymap)
			if (and (consp def)
				(not (eq (car def) t)))
			  return (car def)
			finally return nil)))
    (if last-key
	(define-key-after keymap key def last-key)
      (define-key keymap key def))))

(defun vcmd-bind-menu-FSF (keymap binding &rest menu)
  "Define the menu entry descripbed by MENU in KEYMAP to BINDING.
If (car MENU) is a consp, use (car MENU) as menu list, otherwise use
MENU."
  (setq menu (apply 'append
		    (mapcar '(lambda (x)
			       (if (consp x)
				   x
				 (list x)))
			    menu)))
  (while (cdr menu)
    (let* ((menu-symbol (if (symbolp (car menu))
			    (car menu)
			  (intern (car menu))))
	   (menu-name (if (symbolp (car menu))
			  (symbol-name (car menu))
			(car menu)))
	   (next-keymap (lookup-key keymap (vector menu-symbol))))
      (if next-keymap
	  (setq keymap next-keymap)
	(define-key-last 
	  keymap 
	  (vector menu-symbol) 
	  (cons menu-name
		(setq next-keymap (make-sparse-keymap menu-name))))
	(setq keymap next-keymap))
      (setq menu (cdr menu))))
  (if menu
      (if (eq (car menu) 'separator)
	  (define-key-last 
	    keymap 
	    (vector (vcmd-get-symbol nil)) 
	    '("--" . nil))
	(if binding
	    (if (symbolp (car menu))
		(define-key-last
		  keymap
		  (vector (car menu))
		  (cons (symbol-name (car menu))
			binding))
	      (define-key-last
		keymap
		(vector (intern (car menu)))
		(cons (car menu)
		      binding)))))))

(defun vcmd-bind-menu-lucid (keymap binding &rest menu)
  "Define the menu entry descripbed by MENU in KEYMAP to BINDING.
If (car MENU) is a consp, use (car MENU) as menu list, otherwise use
MENU."
  (setq menu (apply 'append
		    (mapcar '(lambda (x)
			       (if (consp x)
				   x
				 (list x)))
			    menu)))
  (setq menu (nreverse (cdr menu)))
  (apply 'add-menu-button
	 (reverse (cdr menu))
	 (list (vector (if (eq (car menu) 'separator)
			   "--"
			 (if (symbolp (car menu))
			     (symbol-name (car menu))
			   (car menu)))
		       binding
		       ':active t)))
  (set-menubar-dirty-flag))

(defun vcmd-bind-key (keymap binding sequence)
  (if binding
      (define-key keymap sequence binding)))

(defun vcmd-bind-entry (keymap sequence command forms menu) 
  "Bind SEQUENCE and MENU in KEYMAP to COMMAND. Before calling
COMMAND, FORMS will be executed."
  (let ((fn (if command (apply 'vcmd-get-symbol command forms))))
    (if sequence
	(vcmd-bind-key keymap fn sequence))
    (if menu
	(let ((symbol (if (symbolp (car menu))
			  (car menu)
			(intern (car menu)))))
	  (vcmd-bind-menu keymap fn (cons 'menu-bar menu))))))

(defun vcmd-define-key (keymap sequence command &rest menu)
  "Bind key SEQUENCE in KEYMAP to COMMAND indirectly using vcmd."
  (vcmd-bind-entry keymap sequence command nil menu))

(defun vcmd-global-set-key (sequence command &rest menu)
  "Bind SEQUENCE to COMMAND and possibly MENU in the global keymap.

See vvmd-define-key for further documentation."
  (apply 'vcmd-define-key global-map sequence command menu))

;;;###autoload
(defun vcmd-define-menu (keymap sequence commands &rest menu)
  "COMMANDS must be a list of lists of the form

    (tag menu command)

SEQUENCE is bound to a function, which alows the user to select a
tag. The menu entries of the commands will appear as a submenu under
MENU."
  (let ((menu-keymap (and (car menu) (make-sparse-keymap (car menu)))))
    (if (and menu-keymap commands)
	(progn
	  (loop for command in (reverse commands)
		do (define-key 
		     menu-keymap
		     (vector (intern (car command)))
		     (cons (cadr command)
			   (vcmd-get-symbol (caddr command)))))
	  (if sequence
	      (setf (car (last menu))
		    (concat (car (last menu))
			    "    ("
			    (key-description sequence)
			    ")")))
	  (vcmd-bind-menu keymap
			  menu-keymap
			  (cons 'menu-bar menu))))
    (if (and sequence commands)
	(vcmd-bind-key global-map
		       `(lambda () (interactive) (vcmd-select-command ',commands))
		       sequence))))

(defun vcmd-select-command (commands)
  (let ((tag (completing-read "Command: "
			      (mapcar (function (lambda (x) (cons (car x) (cadr x))))
				      commands)
			      nil t)))
    (vcmd (caddr (assoc tag commands)) nil t)))

(defun vcmd-handler (type handler)
  "Install HANDLER as handler for TYPE.

HANDLER must be a function callable with two arguments, the additional
arguments from the vcmd call and an interactive flag, which is set
on interactive call."
  (let ((h (assq type vcmd-handlers)))
    (if h
	(setcdr h handler)
      (setq vcmd-handlers (cons (cons type handler)
				vcmd-handlers)))))

(defun vcmd-flag-handler (flag handler)
  "Install HANDLER as flag-handler for FLAG.

HANDLER must be a function callable with two arguments: the definition
of a vcmd binding and an aditional list of lisp-forms to evaluate
before the vcmd binding. The return value of HANDLER must be a lisp
form evaluating the above mentioned expressions. Normally HANDLER will
call vcmd-encapsulate-fn on its arguments and wrap the result into
additional lisp forms."
  (let ((h (assq flag vcmd-flag-handlers)))
    (if h
	(setcdr h handler)
      (setq vcmd-flag-handlers (cons (cons flag handler)
				     vcmd-flag-handlers)))))

(fset 'vcmd-bind-menu
      (if (string-match "XEmacs" emacs-version)
	  (symbol-function 'vcmd-bind-menu-lucid)
	(symbol-function 'vcmd-bind-menu-FSF)))

(provide 'varcmd)


;;; Local Variables:
;;; elisp-project-autoload-file-name: "varcmd-autoload.el"
;;; End:
