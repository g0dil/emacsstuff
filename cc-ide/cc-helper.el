;;; cc-helper.el --- helper and generator functions for C++
;;
;; $Id$
;;
;; Copyright (C) 2000 Stefan Bund

;; cc-helper.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; cc-helper.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;;; Change-Log:

;; $Log$
;;

;;; Variables:

(defvar c-max-def-column 95
  "*Maximum length of generated argdef lines")

(defconst c-special-key "\\(static\\|virtual\\|friend\\|explicit\\)\\b")

;;; Code:

(require 'cc-engine-2)
(require 'cc-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generators (functions generating new sourcecode)

(defun c-kill-special-keywords ()
  ;; kill all keywords in c-special-key directly after point
  (c-forward-syntactic-ws)
  (while (looking-at c-special-key)
    (delete-region (point) (match-end 1))
    (delete-region (point) (progn (c-forward-syntactic-ws) (point)))))

(defun c-build-template-specs (templates cbuf)
  ;; build temlate specs for TEMPLATES
  (loop for template in templates
	do (insert 
	    (save-excursion
	      (set-buffer cbuf)
	      (save-excursion
		(goto-char (car template))
		(let ((args (c-parse-template-declaration)))
		  (if args
		      (concat "template <"
			      (loop for arg in args
				    for sep = "" then ", "
				    concat sep
				    concat (progn
					     (buffer-substring-no-properties
					      (car arg) (if (c-move-to-initializer 
							     (car arg) (cdr arg))
							    (progn 
							      (forward-char -1)
							      (c-backward-syntactic-ws)
							      (point))
							  (cdr arg)))))
			      ">\n")
		    "")))))))

(defun c-build-defun (&optional add-words no-kill)
  ;; build a function definition header for the current defun. if
  ;; ADD-WORDS is non-nil, it is prepended to the definition header
  ;; after any template specifications. the return value is a cons of
  ;; the name of the function and the complete text of the header.
  ;; c-build-defun tries hard to keep the with of the declaration
  ;; below c-max-def-column
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let* ((tbuf (get-buffer-create " *cc-temp-buffer*"))
	   (state (c-get-defun-state))
	   (prefix (c-get-full-prefix (aref state 8)))
	   (cbuf (current-buffer))
	   p1 p2 p3 c maxc fname)
      (set-buffer tbuf)
      (erase-buffer)
      (c-build-template-specs (aref state 0) cbuf)
      (if (aref state 1)
	  (progn
	    (save-excursion
	      (insert-buffer-substring cbuf (car (aref state 1)) (cdr (aref state 1)))
	      (insert " "))
	    (if (not no-kill)
		(progn
		  (c-kill-special-keywords)))))
      (if add-words
	  (progn
	    (insert add-words " ")))
      (goto-char (point-max))
      (setq p1 (point))
      (insert prefix)
      (if (> (length prefix) 0)
	  (progn
	    (insert "::")
	    (setq p2 (point))))
      (save-excursion
	(insert-buffer-substring cbuf (car (aref state 2)) (cdr (aref state 2))))
      (save-excursion
        (while (re-search-forward "\\s-+" nil t)
          (replace-match "")))
      (if (not p2)
	  (progn
            (c-with-temporary-syntax-table c-mode-syntax-table
              (setq p2 (car (last (c-forward-scoped-name))))
              (if p2
                  (setq p2 (+ p2 2))))))
      (goto-char (point-max))
      (setq fname (buffer-substring-no-properties p1 (point)))
      (if (> (current-column) c-max-def-column)
	  (progn
	    (goto-char p1)
	    (delete-char -1)
	    (insert "\n")
	    (end-of-line)
	    (setq p1 nil)))
      (insert "(")
      (setq p3 (point))
      (setq c (current-column)
	    maxc 0)
      (insert "\n")
      (loop for arg in (aref state 3)
	    for next = nil then t
	    if next do (insert ",\n")
	    do (progn
		 (save-excursion
		   (insert-buffer-substring cbuf (car arg) (cdr arg)))
		 (save-excursion
		   (if (search-forward "=" nil t)
		       (progn
			 (forward-char -1)
			 (c-backward-syntactic-ws)
			 (forward-char 1)
			 (delete-region (1- (point)) (progn (end-of-line) (point))))))
		 (replace-regexp "\\s-+" " ")
		 (end-of-line)
		 (let ((cc (current-column)))
		   (if (> cc maxc)
		       (setq maxc cc)))))
      (if (> (+ c maxc) c-max-def-column)
	  (progn
	    (if (and p1
		     (progn
		       (goto-char p1)
		       (> (1- (current-column))
			  (- (+ c maxc) c-max-def-column))))
		(progn
		  (delete-char -1)
		  (insert "\n"))
	      (if p2
		  (progn
		    (goto-char p2)
		    (insert "\n")
		    (setq p3 (1+ p3)))))))
      (goto-char p3)
      (setq c (current-column))
      (loop for next = nil then t
	    for p = (point)
	    while (not (eobp))
	    do (progn
		 (if next (insert " "))
		 (delete-char 1)
		 (end-of-line)
		 (if (and next (> (current-column) c-max-def-column))
		     (progn
		       (goto-char p)
		       (delete-char 1)
		       (insert "\n" (make-string c ? ))
		       (end-of-line)))))
      (insert ")")
      (if (aref state 4)
	  (progn
	    (insert "\n    ")
	    (save-excursion
	      (insert-buffer-substring cbuf (car (aref state 4)) (cdr (aref state 4))))
	    (replace-regexp "\\s-+" " ")
	    (end-of-line)))
      (if (aref state 5)
	  (progn
	    (insert "\n    : ")
	    (loop with first = t
	          for initializer in (aref state 5)
		  for next = nil then t
		  for p = (point)
		  do (progn
		       (if next (insert ", "))
		       (save-excursion
			 (insert-buffer-substring cbuf (car initializer) 
						  (cdr initializer)))
		       (replace-regexp "\\s-+" " ")
		       (end-of-line)
		       (if (not first)
			   (if (> (current-column) c-max-def-column)
			       (progn
				 (goto-char (1+ p))
				 (delete-char 1)
				 (insert "\n      ")
				 (setq first t)
				 (end-of-line)))
			 (setq first nil))))))
      (prog1
	  (list fname 
		(buffer-substring-no-properties (point-min) (point-max))
		state)
	(kill-buffer tbuf)))))

(defun c-build-create-constructor ()
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let* ((tbuf (get-buffer-create " *cc-temp-buffer*"))
	   (cbuf (current-buffer))
	   (state (c-get-defun-state))
	   (indent (make-string (current-indentation) ? ))
	   (fname (buffer-substring-no-properties (car (aref state 2))
						  (cdr (aref state 2)))))
      (set-buffer tbuf)
      (if (aref state 1)
	  (error "Not a constructor"))
      (insert indent "static ptr create(")
      (let ((indent2 (make-string (current-column) ? ))
	    ml)
	(save-excursion
	  (insert "\n")
	  (loop for arg in (aref state 3)
		for next = nil then t
		if next do (insert ",\n")
		do (progn
		     (save-excursion
		       (insert-buffer-substring cbuf (car arg) (cdr arg)))
		     (replace-regexp "\\s-+" " ")
		     (end-of-line))))
	(loop for next = nil then t
	      for p = (point)
	      while (not (eobp))
	      do (progn
		   (if next (insert " "))
		   (delete-char 1)
		   (end-of-line)
		   (if (and next (> (current-column) c-max-def-column))
		       (progn
			 (setq ml t)
			 (goto-char p)
			 (delete-char 1)
			 (insert "\n" indent2)
			 (end-of-line)))))
	(insert ")");
	(if (aref state 4)
	    (progn
	      (insert " ")
	      (let ((p (point)))
		(save-excursion
		  (insert-buffer-substring cbuf 
					   (car (aref state 4))
					   (cdr (aref state 4))))
		(replace-regexp "\\s-+" " ")
		(end-of-line)
		(if (or ml (> (current-column) c-max-def-column))
		    (progn
		      (goto-char p)
		      (insert "\n" indent (make-string c-basic-offset ? ))
		      (end-of-line))))))
	(insert ";\n"))
      (prog1
	  (list "create"
		(buffer-substring-no-properties (point-min) (point-max))
		state)
	(kill-buffer tbuf)))))

(defun c-build-create-constructor-impl (&optional add-words no-kill)
  (save-excursion
    (let* ((proto (c-build-defun add-words no-kill))
	   (cbuf (current-buffer))
	   (tbuf (get-buffer-create " *cc-temp-buffer*"))
	   (indent (make-string c-basic-offset ? )))
      (set-buffer tbuf)
      (erase-buffer)
      (insert (nth 1 proto) "\n{\n" indent "return ptr(new "
	      (save-excursion
		(set-buffer cbuf)
		(c-scope-name (aref (car (last (aref (nth 2 proto) 8))) 1)))
	      "(")
      (let ((indent2 (make-string (current-column) ? )))
	(save-excursion
	  (insert "\n")
	  (loop for arg in (aref (nth 2 proto) 3)
		for next = nil then t
		if next do (insert ",\n")
		do (insert (save-excursion
			     (set-buffer cbuf)
			     (c-get-template-argument-name (car arg) (cdr arg))))))
	(loop for next = nil then t
	      for p = (point)
	      while (not (eobp))
	      do (progn
		   (if next (insert " "))
		   (delete-char 1)
		   (end-of-line)
		   (if (and next (> (current-column) c-max-def-column))
		       (progn
			 (goto-char p)
			 (delete-char 1)
			 (insert "\n" indent2)
			 (end-of-line)))))
	(insert "));\n}\n"))
      (prog1
	  (list (car proto)
		(buffer-substring-no-properties (point-min) (point-max))
		(cdr proto))
	(kill-buffer tbuf)))))

(eval-when-compile (autoload 'ccide-reformat-defun "cc-ide"))

(defun c-build-default-funcions-impl ()
  (save-excursion
    (let* ((scope (c-get-block-scope))
	   (templates (c-get-templates scope))
	   (prefix (c-get-full-prefix scope))
	   (class (c-parse-class scope))
	   (bases (c-get-base-classes class))
	   (variables (c-get-variable-members class))
	   (name (c-scope-name (aref (car (last scope)) 1)))
	   (in (make-string c-basic-offset ? ))
	   (cbuf (current-buffer))
	   (tbuf (get-buffer-create " *cc-temp-buffer-2*"))
	   template-specs)
      (set-buffer tbuf)
      (erase-buffer)
      (c-build-template-specs templates cbuf)
      (setq template-specs (buffer-substring (point-min) (point-max)))
      (save-excursion 
	(insert "prefix_ " prefix "::" name "()\n")
        (if ccide-gen-throw
            (insert in "throw_(())\n"))
        (insert "{}\n"
                "\n"))
      (ccide-reformat-defun)
      (goto-char (point-max))
      (insert template-specs)
      (save-excursion
	(insert "prefix_ " prefix "::" name "(const " name "& other)\n")
        (if ccide-gen-throw
            (insert in "throw_(())\n"))
        (insert in ": " 
		(mapconcat (function (lambda (x) (concat x "(other)")))
			   bases ", ")
		(if (and bases variables) ", " "")
		(mapconcat (function (lambda (x) (concat x "(other." x ")")))
			   variables ", ")
		"\n{}\n\n"))
      (ccide-reformat-defun)
      (goto-char (point-max))
      (insert template-specs)
      (save-excursion
	(insert "prefix_ " prefix " & " prefix "::operator=(const " 
		name "& other)\n")
        (if ccide-gen-throw
            (insert in "throw_(())\n"))
        (insert "{\n"
		(mapconcat (function (lambda (x) 
				       (concat in "*((" x "*)this) = other;\n")))
			   bases "")
		(mapconcat (function (lambda (x)
				       (concat in x " = other." x ";\n")))
			   variables "")
                in "return *this"
		"}\n\n"))
      (ccide-reformat-defun)
      (goto-char (point-max))
      (insert template-specs)
      (save-excursion
	(insert "prefix_ " prefix "::~" name "()\n{}\n"))
      (ccide-reformat-defun)
      (prog1
	  (list prefix
		(buffer-substring-no-properties (point-min) (point-max)))
	(kill-buffer tbuf)))))

(provide 'cc-helper)


;;; Local Variables:
;;; elisp-project-autoload-file-name: "cc-autoload.el"
;;; End:
