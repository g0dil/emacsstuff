;;; cc-engine-2.el --- Extensuions to cc-engine.el
;;
;; $Id$
;;
;; Copyright (C) 2000 Stefan Bund

;; cc-engine-2.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; cc-engine-2.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;;; Change-Log:

;; $Log$
;;

;;; Variables:

(defconst c-template-arglist-syntax
  (let ((table (copy-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table))

(defconst c-any-key 
  (eval-when-compile
    (concat (regexp-opt '("break" "continue" "do" "else" "for" "if"
			  "return" "switch" "while" "sizeof" "typedef"
			  "extern" "auto" "register" "static" "friend"
			  "volatile" "const" "restrict" "enum"
			  "struct" "union" "class" "char" "short"
			  "int" "long" "signed" "unsigned" "float"
			  "double" "void" "complex" "case" "goto"
			  "inline" "try" "catch" "throw" "inline_"
			  "throw_" "virtual" "new" "delete" "explicit" 
                          "prefix_" "typename" "template") t)
	    "\\b[^_]")))

(defconst c-blocking-key
  (eval-when-compile
    (concat (regexp-opt '("if" "while" "for" "switch")) "\\b[^_]")))

(defconst c-class-scope-key "\\(class\\|struct\\|union\\)\\b[^_]")
(defconst c-namespace-scope-key "namespace\\b[^_]")
(defconst c-scope-key "\\(class\\|struct\\|union\\|namespace\\)");\\b[^_]")
(defconst c-struct-scope-key "struct\\b[^_]")
(defconst c-template-key "template\\b[^_]")
(defconst c-operator-key "operator\\b[^_]")
(defconst c-operator-operators nil)
(defconst c-typedef-key "typedef\\b[^_]")
(defconst c-friend-key "friend\\b[^_]")
(defconst c-access-key "\\(public\\|protected\\|private\\)\\s-*:")
(defconst c-access-keys
  '(("public\\s-*:" . public)
    ("protected\\s-*:" . protected)
    ("private\\s-*:" . private)))
(defconst c-inheritance-spec-key "\\(public\\|protected\\|private\\|virtual\\)\\b[^_]")

(let ((assable '("+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" ">>" "<<"))
      (others '("&&" "||" "++" "--" "->*" "," "->" "[]" "()" "new" "new[]"
		"delete" "delete[]" "bool")))
  (setq c-operator-operators
	(regexp-opt (nconc (mapcar (function (lambda (x) (concat x "=")))
				   assable)
			   assable others) t)))

(defconst c-operator-word 
  (concat "operator\\s-*" c-operator-operators))

(defconst c-skip-syntaxes '(?  ?. ?'))

;;; Code:

(require 'cl)
(require 'cc-engine)
(require 'cc-langs)
(require 'cc-defs)

(defmacro c-with-temporary-syntax-table (table &rest body)
  ;; evaluate BODY temporarily binding the syntax table to TABLE
  (let ((saved-syntax-table (make-symbol "saved-syntax-table")))
    `(let ((,saved-syntax-table (syntax-table)))
       (unwind-protect
	   (progn
	     (set-syntax-table ,table)
	     ,@body)
	 (set-syntax-table ,saved-syntax-table)))))

(def-edebug-spec c-with-temporary-syntax-table (sexp body))
(put 'c-with-temporary-syntax-table 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moving by syntactic entities

(defun c-skip-non-sexp-chars-forward ()
  ;; skip char's not considered part of sexps forward
  (c-forward-syntactic-ws)
  (while (and (not (eq (following-char) ?<))
	      (memq (char-syntax (following-char)) c-skip-syntaxes)
	      (not (eobp)))
    (forward-char 1)
    (c-forward-syntactic-ws)))

(defun c-skip-non-sexp-chars-backward ()
  ;; skip char's not considered part of sexps backward
  (c-backward-syntactic-ws)
  (while (and (not (eq (preceding-char) ?>))
	      (memq (char-syntax (preceding-char)) c-skip-syntaxes)
	      (not (bobp)))
    (forward-char -1)
    (c-backward-syntactic-ws)))

;; support for teplate arglists

(defun c-forward-template-arglist ()
  ;; skip forward over the <> delimited template arglist at
  ;; point. This temporarily changes the syntax-table to include <> as
  ;; matching delimiters and uses c-forward-sexp
  (c-skip-non-sexp-chars-forward)
  (if (not (eq (following-char) ?<))
      (c-forward-sexp)
    (let ((level 1))
      (forward-char 1)
      (while (and (> level 0)
		  (re-search-forward "[[({<>]" nil t))
	(if (not (c-in-literal))
	    (cond ((memq (preceding-char) '(?\[ ?\( ?{))
		   (up-list 1))
		  
		  ((eq (preceding-char) ?<)
		   (setq level (1+ level)))
		  
		  ((eq (preceding-char) ?>)
		   (setq level (1- level)))))))))

(defun c-backward-template-arglist ()
  ;; reverse of c-forward-template-arglist
  (c-skip-non-sexp-chars-backward)
  (if (not (eq (preceding-char) ?>))
      (c-backward-sexp)
    (let ((level 1))
      (forward-char -1)
      (while (and (> level 0)
		  (re-search-backward "[])}<>]" nil t))
	(if (not (c-in-literal))
	    (cond ((memq (following-char) '(?\] ?\) ?}))
		   (up-list -1))

		  ((eq (following-char) ?>)
		   (setq level (1+ level)))
		  
		  ((eq (following-char) ?<)
		   (setq level (1- level)))))))))

(defsubst c-at-symbol-p ()
  (memq (char-syntax (following-char)) '(?w ?_)))

(defsubst c-after-symbol-p ()
  (memq (char-syntax (preceding-char)) '(?w ?_)))

(defun c-forward-extended-sexp ()
  ;; Move forward one sexp. This function tries to correctly skip
  ;; template argument lists delimited by angle brackets. 
  (c-skip-non-sexp-chars-forward)
  (if (and (eq (following-char) ?<)
	   (condition-case nil
	       (let ((start (point)))
		 (c-forward-template-arglist)
		 (if (or (not (eq (preceding-char) ?>))
			 (c-crosses-statement-barrier-p start (point)))
		     (progn (goto-char start) nil) t))
	     (error nil)))
      nil
    (c-forward-sexp)))

(defun c-backward-extended-sexp ()
  ;; reverse of c-forward-extenden-sexp
  (c-skip-non-sexp-chars-backward)
  (if (and (eq (preceding-char) ?>)
	   (condition-case nil
	       (let ((start (point)))
		 (c-backward-template-arglist)
		 (if (or (not (eq (following-char) ?<))
			 (c-crosses-statement-barrier-p (point) start))
		     (progn (goto-char start) nil) t))
	     (error nil)))
      nil
    (c-backward-sexp)))

;; names

(defun c-forward-scoped-name ()
  ;; skip forward over a possibly fully scoped name at point
  ;; optionally containing template arglists. return list of scope
  ;; separators in the name
  (c-forward-syntactic-ws)
  (let (points)
    (while 
	(progn 
	  (setq points (cons (point) points))
	  (if (looking-at "::")
	      (forward-char 2))
	  (c-forward-syntactic-ws)
	  (if (and (cond ((looking-at c-operator-word)
                          (goto-char (match-end 0)))
                         ((looking-at "~")
                          (forward-char 1)
                          (prog1
                              (c-at-symbol-p)
                            (c-forward-token-1)))
                         (t
                          (prog1
                              (c-at-symbol-p)
                            (c-forward-token-1))))
		   (eq (following-char) ?<))
	      (progn
		(c-forward-template-arglist)
		(c-forward-syntactic-ws)))
	  (looking-at "::")))
    (nreverse points)))

(defun c-backward-scoped-name ()
  ;; reverse of c-forward-scoped-name
  (c-backward-syntactic-ws)
  (while
      (progn
	(if (and (eq (preceding-char) ?>)
		 (not (save-excursion
			(re-search-backward (concat c-operator-word "\\=") nil t))))
	    (c-backward-template-arglist))
	(c-backward-syntactic-ws)
	(if (re-search-backward (concat c-operator-word "\\=") nil t)
	    (goto-char (match-beginning 0))
	  (c-backward-token-1)
          (if (and (c-at-symbol-p)
                   (eq (preceding-char) ?~))
              (forward-char -1)))
        (c-backward-syntactic-ws)
	(if (eq (preceding-char) ?:)
	    (progn
	      (forward-char -1)
	      (if (eq (preceding-char) ?:)
		  (progn
		    (forward-char -1)
		    (c-backward-syntactic-ws)
		    t)
		(forward-char 1)
		nil)))))
  (c-forward-syntactic-ws))

(defun c-forward-balanced-token ()
  (c-forward-syntactic-ws)
  (cond ((or (c-at-symbol-p)
	     (looking-at c-operator-word))
	 (c-forward-scoped-name))
	((memq (following-char) '(?\( ?{ ?<))
	 (c-forward-extended-sexp))
	(t
	 (c-forward-token-1))))

(defun c-backward-balanced-token ()
  (c-backward-syntactic-ws)
  (cond ((or (c-after-symbol-p)
	     (re-search-backward (concat c-operator-word "\\=") nil t))
	 (c-backward-scoped-name))
	((memq (preceding-char) '(?\) ?} ?>))
	 (c-backward-extended-sexp))
	(t
	 (c-backward-token-1))))

;; defun's

(defun c-move-to-start-of-defun (&optional limit)
  ;; move point to start of current defun. point is left at the start
  ;; of the function's name. Use (c-beginning-of-statement-1) to get
  ;; to the start of the declaration. returns point of body's opening
  ;; brace if defun found, otherwise nil. if LIMIT is non-nil, don't
  ;; move farther back than that.
  (let (new-point brace-point)
    (save-excursion
      (while 
	  (and (c-save-uplist -1)
	       (or (not limit)
		   (> (point) limit))
	       (not (setq new-point
			  (if (and (eq (following-char) ?{)
				   (c-just-after-func-arglist-p))
			      (progn
				(setq brace-point (point))
				(c-beginning-of-statement-1)
				(while (and (< (point) brace-point)
					    (not (eq (following-char) ?\()))
				  (c-forward-extended-sexp)
				  (c-forward-syntactic-ws))
				(if (eq (following-char) ?\()
				    (progn
				      (c-backward-syntactic-ws)
				      (c-backward-scoped-name)
				      (if (not (looking-at c-conditional-key))
					  (point)))))))))))
    (if new-point
	(goto-char new-point))
    (and new-point brace-point)))

(defun c-beginning-of-defun-or-decl ()
  (c-move-to-start-of-defun)
  (let ((point (point)) beg)
    (c-beginning-of-statement-1)
    (setq beg (point))
    (c-end-of-statement-1)
    (if (> (point) point)
	(goto-char beg)
      (goto-char point))
    (c-forward-syntactic-ws)))

(defun c-forward-out-of-comment ()
  (while (memq (c-in-literal) '(c c++))
    (forward-char 1)))

(defun c-beginning-of-statement-2 ()
  ;; Move to the REAL beginning of the statement, ignoring all subexpressions
  (let ((point (point))
	(state (c-parse-state))
	(last (point)))
    (while (and state
		(not (consp (car state)))
		(progn
		  (goto-char (car state))
		  (looking-at "(")))
      (setq last (car state)
	    state (cdr state)))
    (if (and state last
	     (not (consp (car state))))
	(goto-char last))
    (c-beginning-of-statement-1)
    (while (and (< (point) point)
		(or (c-crosses-statement-barrier-p (point) point)
		    (not (equal (c-parse-state) state))))
      (c-end-of-statement-1))
    (c-forward-syntactic-ws)
    (while (looking-at c-any-key)
      (if (looking-at c-blocking-key)
          (progn
            (c-forward-token-1)
            (c-forward-sexp))
        (c-forward-token-1))
      (c-forward-syntactic-ws))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; information on scopes (nesting levels)

(defun c-aggressive-search-uplist-for-classkey ()
  ;; like search-uplist-for-classkey but agressively retry at all
  ;; scoping levels until classkey found
  (save-excursion
    (let (state)
      (loop for state = (c-parse-state)
            while state
            thereis (loop for substate on state
                          thereis (c-search-uplist-for-classkey substate))
            for elt = (car (last state))
            do (goto-char (if (consp elt) (car elt) elt))))))

(defun c-search-uplist-for-scopekey (state)
  (let ((c-class-key c-scope-key))
    (c-search-uplist-for-classkey state)))

(defun c-aggressive-search-uplist-for-scopekey ()
  (let ((c-class-key c-scope-key))
    (c-aggressive-search-uplist-for-classkey)))

(defun c-save-uplist (arg)
  ;; like up-list but return nil on error
  (condition-case nil
      (progn
	(up-list arg)
	(point))
    (scan-error nil)))

(defun c-full-parse-state ()
  ;; return the complete parse-state from the beginning of buffer up
  ;; to (point)
  (save-excursion
    (let (state s elt)
      (while (setq s (c-parse-state)
                   elt (car (last s)))
        (goto-char (if (consp elt) (car elt) elt))
        (setq state (nconc state s)))
      state)))

(defun c-get-block-scope ()
  ;; return a list of scoping levels for point. Every scoping level is
  ;; identified by thier 'class for a class scope, or 'namespace for a
  ;; namespace scope For 'class and 'struct scopes, optional template
  ;; declarations are returned.
  (save-excursion
    (let (key element keys)
      (while (setq key (c-aggressive-search-uplist-for-scopekey))
	(goto-char (aref key 0))
	(setq element (vector nil
			      (aref key 0)
			      (aref key 1)
			      nil))
	(cond ((looking-at c-class-scope-key)
	       (aset element 0 'class)
	       (c-backward-syntactic-ws)
	       (if (eq (preceding-char) ?>)
		   ;; this is a templated class/struct declaration
		   (save-excursion
		     (c-backward-template-arglist)
		     (c-backward-token-1)
		     (if (looking-at c-template-key)
			 (aset element 3 (point))))))

	       ((looking-at c-namespace-scope-key)
		(aset element 0 'namespace)))

	(if (aref element 0)
	    (setq keys (cons element keys))))
      keys))) 

(defun c-get-scope ()
  ;; This is like c-get-block-scope. Additionaly, if in a function
  ;; declaration or definition this will add a 'defun entry at the
  ;; end detailing the function information (and having an optional
  ;; template spec). The start of the function entry is the first char
  ;; of the functions typespec, the last char is just after the
  ;; closing paren of the function defn or decl.
  (let ((scope (c-get-block-scope)))
    (save-excursion
      (if (c-move-to-start-of-defun (and scope (aref (car (last scope)) 1)))
	  (let ((element (vector 'defun (point) nil nil)))
	    (c-forward-scoped-name)
	    (aset element 2 (point))
	    (c-beginning-of-statement-1)
	    (if (looking-at c-template-key)
		(aset element 3 (point)))
	    (nconc scope (list element)))
	scope))))

(defun c-scope-name (p &optional strip)
  ;; return the name of the scope at P. if STRIP is non-nil, strip
  ;; that many elements from the name
  (save-excursion
    (goto-char p)
    (if (looking-at c-scope-key)
	(c-forward-token-1))
    (let ((points (c-forward-scoped-name)))
      (c-backward-syntactic-ws)
      (buffer-substring-no-properties (car points)
				      (or (and strip (> strip 0)
					       (or (and (<= strip (length points))
							(car
							 (last 
							  (nbutlast points 
								    (1- strip)))))
						   (car points)))
					  (point))))))

(defun c-get-class-at-point ()
  ;; Return block scope for class at point.
  (save-excursion
    (c-forward-syntactic-ws)
    (while (looking-at c-template-key)
      (goto-char (match-end 0))
      (c-forward-extended-sexp)
      (c-forward-syntactic-ws))
    (and (looking-at c-class-scope-key)
	 (search-forward "{" nil t))
    (last (c-get-block-scope))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template functions

(defun c-parse-template-declaration ()
  ;; parse the template declaration at point. return a list of
  ;; cons'es of argument def ranges.
  (save-excursion
    (if (looking-at c-template-key)
	(c-forward-token-1)
      (c-forward-syntactic-ws))
    (if (eq (following-char) ?<)
	(c-parse-arglist (point) 
			 (progn (c-forward-template-arglist) (point))))))

(defun c-parse-arglist (start end)
  ;; parse arglist between START and END. The region between START end
  ;; END must include the delimiteres (parens or angle brackets) even
  ;; though theese delimiters are completely ignored
  (setq end (1- end))
  (let (args arg-start)
    (save-excursion
      (goto-char start)
      (while (and (not (eobp))
		  (< (point) end))
	(forward-char 1)
	(c-forward-syntactic-ws)
	(setq arg-start (point))
	(condition-case nil
	    (while (progn
		     (c-forward-extended-sexp)
		     (and (not (eobp))
			  (< (point) end)
			  (not (eq (following-char) ?,)))))
	  (scan-error nil))
	(save-excursion
	  (c-backward-syntactic-ws)
	      (if (> (point) end)
		  (goto-char end))
	      (if (> (point) arg-start)
		  (setq args (cons (cons arg-start (point))
				   args))))))
    (nreverse args)))

(defun c-move-to-template-argument (start end)
  ;; move to the template argument name within the template argument
  ;; between START and END
  (if (c-move-to-initializer start end)
      (forward-char -1)
    (goto-char end))
  (while (and (>= (point) start)
	      (not (c-at-symbol-p))
	      (not (bobp)))
    (c-backward-extended-sexp))
  (c-at-symbol-p))

(defun c-get-template-argument-name (start end)
  ;; get the argument name of the template argument defined between
  ;; START and END
  (save-excursion
    (c-move-to-template-argument start end)
    (buffer-substring-no-properties (point)
				    (progn
				      (c-forward-token-1)
				      (c-backward-syntactic-ws)
				      (point)))))

(defun c-get-template-prefix (args)
  ;; return the template prefix for the template declared with
  ;; arguments ARGS
  (concat "<"
	  (mapconcat (function (lambda (x) 
				 (c-get-template-argument-name (car x) (cdr x))))
		     args
		     ",")
	  ">"))

(defun c-is-template-id (p)
  ;; return t if scoped name at P is a template_id
  (save-excursion
    (goto-char p)
    (if (looking-at c-scope-key)
	(c-forward-token-1))
    (c-forward-scoped-name)
    (c-backward-syntactic-ws)
    (eq (preceding-char) ?>)))

(defun c-move-to-initializer (start end)
  ;; move point to the initializer for the argument declared between
  ;; START and END. return t if initializer found, otherwise nil. if
  ;; no initializer is found, point is left at START
  (goto-char start)
  (search-forward "=" end t))

(defun c-get-templates (scope)
  ;; return list of ranges of template specs in SCOPE
  (loop for level in scope
	if (aref level 3)
	collect (progn
		  (goto-char (aref level 3))
		  (c-forward-token-1)
		  (c-forward-template-arglist)
		  (c-backward-syntactic-ws)
		  (cons (aref level 3) (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to parse defuns

(defun c-get-full-prefix (scope &optional strip)
  ;; return the full prefix for scope. if STRIP is non-nil, strip the
  ;; name of the current method, if any.
  (save-excursion
    (loop with last-p = (last scope)
	  for elem-p on scope
	  for elem = (car elem-p)
	  for next = nil then t
	  for last = (eq elem-p last-p)
	  if (and last strip (eq (aref elem 0) 'defun))
	    concat (let ((name (c-scope-name (aref elem 1) 1)))
		     (if (> (length name) 0)
			 (concat (if next "::" "") name) ""))
	  else
	    concat (concat (if next "::" "")
			   (c-scope-name (aref elem 1))
			   (if (and (aref elem 3)
				    (not (c-is-template-id (aref elem 1))))
			       (progn
				 (goto-char (aref elem 3))
				 (c-get-template-prefix 
				  (c-parse-template-declaration)))
			     "")))))

(defun c-parse-defun ()
  ;; parse function definition or declaration at point. Returns a vector
  ;; of positions: [template type name arglist modifiers initializers body end]
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let (template type name arglist modifiers initializers body end)
      (if (looking-at c-template-key)
	  (progn
	    (setq template (point))
	    (while (looking-at c-template-key)
	      (c-forward-token-1)
	      (c-forward-template-arglist)
	      (c-forward-syntactic-ws))))
      (setq type (point))
      (while (not (or (eq (following-char) ?\()
		      (c-crosses-statement-barrier-p type (point))))
	(c-forward-balanced-token)
	(c-forward-syntactic-ws))
      (save-excursion
	(c-backward-scoped-name)
	(setq name (point))
	(if (eq name type)
	    (setq type nil)))
      (setq arglist (point))
      (c-forward-sexp)
      (c-forward-syntactic-ws)
      (if (not (memq (following-char) '(?{ ?\; ?:)))
	  (progn
	    (setq modifiers (point))
	    (while (not (or (memq (following-char) '(?{ ?\; ?:))
			    (c-crosses-statement-barrier-p modifiers (point))
                            (eobp)))
	      (c-forward-extended-sexp)
	      (c-forward-syntactic-ws))))
      (if (eq (following-char) ?:)
	  (progn
	    (setq initializers (point))
	    (while (not (or (memq (following-char) '(?{ ?\;))
			    (c-crosses-statement-barrier-p modifiers (point))))
	      (c-forward-extended-sexp)
	      (c-forward-syntactic-ws))))
      (if (eq (following-char) ?{)
	  (progn
	    (setq body (point))
	    (c-forward-sexp)))
      (setq end (point))
      (vector template type name arglist modifiers initializers body end))))

(defun c-get-defun-state ()
  ;; this extends c-parse-defun. it returns a vector containing the
  ;; following items:
  ;;   o templates: a list of cons'es each containing the range of a
  ;;             template specification
  ;;   o type: a cons containing the range for the return type
  ;;             specification of the function
  ;;   o name: a cons containing the range for the functions name
  ;;   o args: a list of cons'es, each containing the range of a
  ;;             function argument
  ;;   o modifiers: a cons containing the range of the modifiers
  ;;   o initializers: a list of cons'es each containing the range of
  ;;             an initializer
  ;;   o body: a cons containing the range for the body or nil, if no
  ;;             body
  ;;   o prototype: nil, if body is non-nil, otherwise the end of the
  ;;             prototype.
  ;;   o scope: the scope structure (as returned by c-get-block-scope)
  ;;             for this function
  (save-excursion
    (let ((defun (c-parse-defun)) 
	  (scope (c-get-block-scope))
	  templates type name args modifiers initializers body prototype)
      (setq templates (c-get-templates scope))
      (if (aref defun 0)
	  (progn
	    (goto-char (aref defun 0))
	    (while (looking-at c-template-key)
	      (setq templates (nconc templates
				     (list (cons (point)
						 (progn
						   (c-forward-token-1)
						   (c-forward-template-arglist)
						   (c-backward-syntactic-ws)
						   (point))))))
	      (c-forward-syntactic-ws))))
      (if (aref defun 1)
	  (progn
	    (goto-char (aref defun 2))
	    (c-backward-syntactic-ws)
	    (setq type (cons (aref defun 1) (point)))))
      (goto-char (aref defun 3))
      (c-backward-syntactic-ws)
      (setq name (cons (aref defun 2) (point)))
      (goto-char (aref defun 3))
      (let ((start (point)))
	(c-forward-sexp)
	(setq args (c-parse-arglist start (point))))
      (if (aref defun 4)
	  (progn
	    (goto-char (or (aref defun 5) (aref defun 6) (aref defun 7)))
	    (c-backward-syntactic-ws)
	    (setq modifiers (cons (aref defun 4) (point)))))
      (if (aref defun 5)
	  (setq initializers (c-parse-arglist (aref defun 5)
					      (1+ (or (aref defun 6)
						      (aref defun 7))))))
      (if (aref defun 6)
	  (setq body (cons (aref defun 6) (aref defun 7))))
      (if (not body)
	  (setq prototype (1+ (aref defun 7))))
      (vector templates type name args modifiers 
	      initializers body prototype scope))))

(defun c-defun-full-name (state)
  ;; return the full name of the defun in state
  (string-replace "[ \t\n\r]+" "" 
		  (concat (c-get-full-prefix (aref state 8))
			  (if (aref state 8) "::" "")
			  (buffer-substring-no-properties (car (aref state 2))
							  (cdr (aref state 2))))
		  t))

(defun c-defun-short-name (state)
  ;; return the short name of the defun in state. This is the name of the defun
  ;; without template args or namespace/class prefix
  (let (p)
    (save-excursion
      (goto-char (cdr (aref state 2)))
      (if (and (eq (preceding-char) ?>)
	       (not (save-excursion
		      (re-search-forward (concat c-operator-word "\\=") nil t))))
	  (c-backward-template-arglist))
      (c-backward-syntactic-ws)
      (setq p (point))
      (if (re-search-backward (concat c-operator-word "\\=") nil t)
	  (goto-char (match-beginning 0))
	(c-backward-token-1)
	(if (and (c-at-symbol-p)
		 (eq (preceding-char) ?~))
	    (forward-char -1)))
      (buffer-substring-no-properties p (point)))))

(defun c-goto-beginning-of-defun (defun)
  (goto-char (or (car (aref defun 1))
		 (car (aref defun 2))))
  (loop for point = (point)
	for tmpl in (reverse (aref defun 0))
	do (c-backward-syntactic-ws)
	while (= (cdr tmpl) (point))
	do (progn
	     (goto-char (car tmpl))
	     (setq point (point)))
	finally do (goto-char point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to parse classes

(defun c-parse-class (scope)
  ;; parse class at point. returns vector of positions: [template
  ;; class bases start ( members )] each member is a cons ( type
  ;; . start ) where type is one of 'typedef 'class 'friend 'variable
  ;; 'method or 'combo (combo is a combinded struct/class/union +
  ;; variable declaration)
  (save-excursion
    (let ((scope (car (last scope)))
	  end template class bases start members)
      (if (not (eq (aref scope 0) 'class))
	  nil
	(setq template (aref scope 3))
	(setq class (aref scope 1))
	(setq start (aref scope 2))
	(goto-char start)
	(while (and (< (skip-chars-backward "^:" class) 0)
		    (or (progn 
			  (forward-char -1)
			  (and (eq (char-before) ?:) (progn (forward-char -1) t)))
			(c-in-literal))))
	(if (eq (following-char) ?:)
	    (progn
	      (forward-char 1)
	      (c-forward-syntactic-ws)
	      (setq bases (point))))
	(goto-char start)
	(save-excursion
	  (c-forward-sexp)
	  (setq end (point)))
	(forward-char 1)
	(while (progn (c-end-of-statement-1)
		      (< (point) end))
	  (let ((bc (char-before))
		(this (point)))
	    (if (eq bc ?{)
		(save-excursion
		  (forward-char -1)
		  (c-forward-sexp)
		  (setq this (point))))
	    (if (or (eq bc ?\;) (eq bc ?{))
		(progn
		  (forward-char -1)
		  (if (re-search-backward "=\\s-*0\\s-*\\=" start t)
		      (goto-char (match-beginning 0)))
		  (if (c-just-after-func-arglist-p)
		      ;; OK. It's a method (defn or decl)
		      (progn
			(c-beginning-of-statement-1)
			(setq members (cons (cons 'method (point))
					    members)))
		    (if (eq bc ?{)
			;; this should be a class or struct decl. Maybe
			;; a variable.
			(let (pos decl beg)
			  (setq pos (point))
			  (c-beginning-of-statement-1)
			  (setq beg (point))
			  (if (looking-at c-class-scope-key)
			      ;; it really IS a class/struct/union
			      (progn
				(goto-char (match-end 0))
				(c-forward-syntactic-ws)
				(setq decl (looking-at "[a-zA-Z_]"))
				(goto-char pos)
				(c-forward-sexp)
				(c-forward-syntactic-ws)
				(if (eq (following-char) ?\;)
				    ;; no variable defn
				    (if decl
					(setq members (cons (cons 'class beg)
							    members)))
				  (save-excursion
				    (goto-char this)
				    (c-end-of-statement-1)
				    (setq this (point)))
				  (setq members (cons (cons (if decl 'combo 'variable)
							    beg)
						      members))))))
		      ;; then it's a variable decl or typedef or friend
		      (c-beginning-of-statement-1)
		      (cond ((looking-at c-typedef-key)
			     (setq members (cons (cons 'typedef (point)) members)))
			    ((looking-at c-friend-key)
			     (setq members (cons (cons 'friend (point)) members)))
			    (t
			     (setq members (cons (cons 'variable (point)) members))))
		      ))))
	    (goto-char this)))
	(vector template class bases start (nreverse members))))))

(defun c-current-access-level ()
  ;; returm current access level: 'public, 'protected or 'private
  (save-excursion
    (let ((scope (car (last (c-get-block-scope)))))
      (while (and (re-search-backward c-access-key (aref scope 2) t)
		  (or (c-in-literal)
		      (not (eq (aref (car (c-get-block-scope)) 1) (aref scope 1))))))
      (loop for (re . sym) in c-access-keys
	    if (looking-at re)
	      return sym
	    finally return (progn 
			     (goto-char (aref scope 1))
			     (if (looking-at c-struct-scope-key)
				 'public
			       'private))))))

(defun c-get-variable-members (class)
  ;; return list of names of all variables of CLASS
  (save-excursion
    (loop for (type . pos) in (aref class 4)
	  for end = (progn (goto-char pos) (c-end-of-statement-1) (1- (point)))
	  if (or (eq type 'variable) (eq type 'combo))
	    collect (c-get-template-argument-name pos end))))

(defun c-get-variable-members-with-type (class)
  ;; return list of conses of (name . type) of all variables of CLASS
  (save-excursion
    (loop for (type . pos) in (aref class 4)
	  for end = (progn (goto-char pos) (c-end-of-statement-1) (1- (point)))
	  if (eq type 'variable)
	    collect (c-get-variable-with-type pos end))))

(defun c-get-variable-with-type (start end)
  (c-move-to-template-argument start end)
  (let ((arg (save-excursion
	       (buffer-substring-no-properties (point)
					       (progn
						 (c-forward-token-1)
						 (c-backward-syntactic-ws)
						 (point))))))
    (c-backward-syntactic-ws)
    (cons arg (buffer-substring-no-properties start (point)))))

(defun c-get-base-classes (class)
  ;; return list of base class names (including template specs)
  (and (aref class 2)
       (save-excursion
	 (goto-char (aref class 2))
	 (loop while (< (point) (aref class 3))
	       do (progn (c-forward-syntactic-ws)
			 (while (looking-at c-inheritance-spec-key)
			   (c-forward-token-1)
			   (c-forward-syntactic-ws)))
	       for start = (point)
	       do (progn (c-forward-scoped-name) (c-backward-syntactic-ws))
	       collect (buffer-substring-no-properties start (point))
	       do (progn
		    (while (and (> (skip-chars-forward "^," (aref class 3)) 0)
				(c-in-literal))
		      (forward-char 1))
		    (forward-char 1))))))

(provide 'cc-engine-2)


;;; Local Variables:
;;; elisp-project-autoload-file-name: "cc-autoload.el"
;;; End:
