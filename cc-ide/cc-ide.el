;;; cc-ide.el --- C++ IDE
;;
;; $Id$
;;
;; Copyright (C) 2000 Stefan Bund

;; cc-ide.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; cc-ide.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;;; Change-Log:

;; $Log$
;;

;;; Variables:

(defvar ccide-compile-opts "DEBUG=1"
  "*Additional options to make command")

(defvar ccide-file-vars nil)

(defvar ccide-default-author "")
(defvar ccide-default-copyright "")

(defvar ccide-project-name nil)

(defvar ccide-all-includes nil
  "*If non-nil, this is the name of a file to include to fetch all
includes of a project. This is used if single include files cannot be
correctly included.")

(defvar ccide-corba-skel-dir "")
(defvar ccide-corba-idl-dir "")
(defvar ccide-corba-idl-command "omniidl2 -w")

(defvar c-user-prefixes '("inline" "static" "prefix_")
  "*List of possible prefixes for function definitions.")

(defvar ccide-default-prefix "prefix_"
  "*Prefix added to every implementation header. Probably eiter empty or 'prefix_'")

(defvar ccide-gen-throw nil
  "*If non-nil, generate throw_ specs")

(defvar ccide-project-root)

(defvar ccide-new-file-hook)

(defconst c-user-prefix-re (regexp-opt c-user-prefixes t))

(defconst ccide-doxy-tag-re 
  (concat "\\\\\\(group\\|defgroup\\|see\\|author\\|version\\|id\\|since"
	  "\\|returns?\\|throws?\\|exception\\|raises\\|param\\|li\\|brief"
	  "\\|internal\\|bug\\|fixme\\|todo\\|idea\\|implementation"
	  "\\|note\\|attention\\|warning\\|par\\|code\\|endcode"
	  "\\|post\\|pre\\|deprecated\\)\\b"))

(defconst ccide-special-extensions
  '(".h" ".hh" ".mpp" ".ih" ".cc" ".cpp" ".ct" ".cti" ".cci" ".dox"))

(defconst ccide-implementation-extensions
  '(".h" ".hh" ".ih" ".cc" ".cpp" ".ct" ".cti" ".cci"))

(defconst ccide-class-defaults-word 
  "// \\(default\\|no\\|protected\\|private\\|disabled\\|my\\)")

(defconst  ccide-bindings
  '(
    ;; file level
    ("fc"  ccide-file-comment		      "File comment")
    ("fs"  ccide-syncronize-includes	      "Sync includes")
    (nil   nil				      separator)
    					  
    ;; class level			  
    ("cc"  ccide-class-comment		      "Class comment")
    ("cg"  ccide-gen-class		      "Generate class")
    ("cd"  ccide-gen-class-defaults	      "Generate class defaults")
    ("cD"  ccide-gen-class-defaults-impl      "Generate class defaults impl")

    ("csd" ccide-set-class-defaults-default   "Set class defaults comment" "Default")
    ("csn" ccide-set-class-defaults-no	      "Set class defaults comment" "No")
    ("csp" ccide-set-class-defaults-protected "Set class defaults comment" "Protected")
    ("csr" ccide-set-class-defaults-private   "Set class defaults comment" "Private")
    ("csx" ccide-set-class-defaults-disabled  "Set class defaults comment" "Disabled")
    ("csm" ccide-set-class-defaults-my	      "Set class defaults comment" "My")

    ("cS"  ccide-gen-struct-constructors      "Generate structure constructors")

    ("ci"  ccide-class-impl-comment           "Generate class implemenation comment")

    ("ce"  ccide-gen-exception                "Generate an exception class")

    (nil   nil				      separator)
    					  
    ;; method level			  
    ("mc"  ccide-function-comment	      "Method comment")
    ("mp"  ccide-grab-prototype		      "Grab prototype")
    ("mr"  ccide-reformat-defun		      "Reformat defun")
    ("mx"  ccide-replace-defun		      "Replace defun")
    ("mt"  ccide-prefix-defun-type-with-class "Prefix defun type with class")
    ("mn"  ccide-prefix-defun-type-with-namespace "Prefix defun type with namespace")
    ("mi"  ccide-grab-inline-decl	      "Grab inline decl")
    ("mA"  ccide-grab-all-inlines             "Grab ALL inline decls")
    ("mC"  ccide-grab-create-constructor      "Grab CREATE constructor")
    ("mI"  ccide-grab-create-constructor-impl "Build CREATE cosntructor")
    ("mf"  ccide-find-implementation          "Find method implementation")
    ("mT"  ccide-insert-defun-prefix          "Insert current defun prefix at point")
    (nil   nil				      separator)
    					  
    ;; variable level			  
    ("vc"  ccide-variable-comment	      "Variable comment")
    ("vf"  ccide-grab-access-fn		      "Grab access methods")
    (nil   nil				      separator)

    ;; documentation
    ("h"  ccide-hide-all-doxy-comments        "Hide all Doxygen comments")
    ("s"  ccide-show-all-comments             "Show all Doxygen comments")
    					  
;    ;; CORBA				  
;    ("Cg"  ccide-gen-corba-impl		      "Generate CORBA impl")
;    ("Cm"  ccide-gen-corba-impl-methods       "Generate CORBA impl methods")
;    (nil   nil				      separator)
    					  
    ;; templates			  
;    ("ts"  ccide-scan-mantemps		      "Scan mantemps")
;    (nil   nil                                separator)

;    ;; other
;    ("of"  ccide-open-compilation-frame       "Open *compilation* frame")
;    ("oc"  ccide-compile-compile              "Make -> Compile")
;    ("ox"  ccide-compile-clean                "Make -> Clean")
;    ("od"  ccide-compile-cleandepends         "Make -> Clean depends")
;    ("ok"  ccide-compile-kill                 "Kill compilation")
;    ("oh"  ccide-hide-compilation             "Hide *compilation* buffer")

    ))

;;; Code:

(require 'cc-engine-2)
(require 'cc-helper)
(require 'c++)
(require 'cl)
(require 'hideshow)
;(require 'mantemp)
(require 'locate)
(require 'lucid)
(require 'varcmd)
(require 'misc-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(defsubst ccide-match-string (n)
  (buffer-substring-no-properties (match-beginning n) (match-end n)))

(defun ccide-file-macro-name (&optional file-name)
  (concat (upcase (file-name-extension (or file-name (buffer-file-name))))
          "_" 
	  (if ccide-project-name (concat ccide-project-name "_") "")
	  (if ccide-project-root
	      (string-replace "/" "_" 
			      (substring (file-name-directory 
					  (expand-file-name (or file-name buffer-file-name)))
					 (length ccide-project-root))
			      t)
	    "")
          (string-replace "\\." "_" (file-name-sans-extension 
                                     (file-name-nondirectory 
                                      (or file-name (buffer-file-name))))
                          t nil t t)
          "_"))

(defun ccide-file-name (&optional extension file-name directory)
  (concat (if directory (file-name-as-directory directory) "")
	  (file-name-sans-extension
	   (file-name-nondirectory 
	    (or file-name (buffer-file-name))))
	  extension))

(defun ccide-in-doxy-comment ()
  (save-excursion
    (back-to-indentation)
    (let ((lit (c-in-literal)))
      (if (cond ((eq lit 'c)
		 (goto-char (car (c-literal-limits)))
		 (looking-at "/\\*\\*<?[ \t\n\r@]"))
		((eq lit 'c++)
		 (goto-char (car (c-literal-limits)))
		 (looking-at "///<?[ \t\n\r@]"))
		(t nil))
	  (progn
	    (goto-char (match-end 0))
	    (current-column))))))

(defun ccide-shell-command (command)
  (let ((obuf (get-buffer-create "*ccide shell command*"))
	exit-status)
    (save-excursion
      (set-buffer obuf)
      (erase-buffer)
      (insert command "\n"))
    (setq exit-status (call-process shell-file-name nil "*ccide shell command*" nil
				    shell-command-switch command))
    (and exit-status (equal exit-status 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file level

(defun ccide-file-comment ()
  "Add a comment to this source file."
  (interactive)
  (let ((mode "c++")
	point add-file-vars)
    (push-mark)
    (goto-char (point-min))
    (insert "// $Id$\n"
	    "//\n"
	    "// Copyright (C) " (number-to-string (nth 5 (decode-time)))
	    " " ccide-default-author "\n"
            ccide-default-copyright
            "\n")

    (cond ((string-match "\\.hh?$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " public header */\n\n"
		   "#ifndef " (ccide-file-macro-name) "\n"
		   "#define " (ccide-file-macro-name) " 1\n\n")
	   (if ccide-all-includes
	       (insert "#ifndef " (ccide-file-macro-name ccide-all-includes) "\n"
		       "#error \"Don't include '" (file-name-nondirectory (buffer-file-name)) "'"
		       " directly, include '" ccide-all-includes "'\"\n"
		       "#endif\n\n"))
	   (insert "// Custom includes\n\n"
                   "//#include \"" (ccide-file-name ".mpp") "\"\n"
                   "///////////////////////////////hh.p////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
	   (insert "\n\n///////////////////////////////hh.e////////////////////////////////////////\n")
	   (if ccide-all-includes
	       (insert "#endif\n"
		       "#if !defined(" (ccide-file-macro-name ccide-all-includes) "_decls_) "
		       "&& !defined(" (ccide-file-macro-name) "i_)\n"
		       "#define " (ccide-file-macro-name) "i_\n"))
	   (insert "//#include \"" (ccide-file-name ".cci") "\"\n"
		   "//#include \"" (ccide-file-name ".ct") "\"\n"
		   "//#include \"" (ccide-file-name ".cti") "\"\n"
		   "#endif"))

	  ((string-match "\\.mpp$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " Boost.Preprocesser external iteration include */\n\n"


		   "#if !BOOST_PP_IS_ITERATING && !defined(" (ccide-file-macro-name) ")\n"
		   "#define " (ccide-file-macro-name) " 1\n\n"
		   "// Custom includes\n\n\n"
		   "// ///////////////////////////mpp.p////////////////////////////////////////\n"
		   "#elif BOOST_PP_IS_ITERATING // ////////////////////////////////////////////\n"
		   "// ////////////////////////////////////////////////////////////////////////\n"
		   "// Local Macros\n\n\n"
		   "// ////////////////////////////////////////////////////////////////////////\n"
		   "#if BOOST_PP_ITERATION_FLAGS()==1 // //////////////////////////////////////\n"
		   "// ////////////////////////////////////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
	   (insert "\n\n// ////////////////////////////////////////////////////////////////////////\n"
		   "#endif // /////////////////////////////////////////////////////////////////\n"
		   "// ////////////////////////////////////////////////////////////////////////\n"
		   "// Undefine local Macros\n\n\n"
		   "// ////////////////////////////////////////////////////////////////////////\n"
		   "#endif // /////////////////////////////////////////////////////////////////\n"
		   "// ///////////////////////////mpp.e////////////////////////////////////////"))

	  ((string-match "\\.ih$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " internal header */\n\n"
		   "#ifndef " (ccide-file-macro-name) "\n"
		   "#define " (ccide-file-macro-name) " 1\n\n"
                   "// Custom includes\n\n"
                   "///////////////////////////////ih.p////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
	   (insert "\n\n///////////////////////////////ih.e////////////////////////////////////////\n"
                   "#endif"))

          ((or (string-match "\\.test\\.cc$" (buffer-file-name))
               (string-match "\\.test\\.cpp$" (buffer-file-name)))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " unit tests */\n\n"
		   "//#include \"" (ccide-file-name ".hh") "\"\n"
		   "//#include \"" (ccide-file-name ".ih") "\"\n\n"
                   "// Custom includes\n"
                   "#include \"" (or ccide-all-includes 
				     (ccide-file-name ".hh" (ccide-file-name))) "\"\n\n"
                   "#include <boost/test/auto_unit_test.hpp>\n"
                   "#include <boost/test/test_tools.hpp>\n\n"
		   "#define prefix_\n"
                   "///////////////////////////////cc.p////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
           (insert "\n\n///////////////////////////////cc.e////////////////////////////////////////\n"
		   "#undef prefix_"))

	  ((or (string-match "\\.cc$" (buffer-file-name))
	       (string-match "\\.cpp$" (buffer-file-name)))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " non-inline non-template implementation */\n\n"
		   (if ccide-all-includes "" "//")
		   "#include \"" (or ccide-all-includes (ccide-file-name ".hh")) "\"\n"
		   "//#include \"" (ccide-file-name ".ih") "\"\n\n"
                   "// Custom includes\n\n"
                   "//#include \"" (ccide-file-name ".mpp") "\"\n"
		   "#define prefix_\n"
                   "///////////////////////////////cc.p////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
           (insert "\n\n///////////////////////////////cc.e////////////////////////////////////////\n"
		   "#undef prefix_\n"
                   "//#include \"" (ccide-file-name ".mpp") "\""))

	  ((string-match "\\.cci$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " inline non-template implementation */\n\n"
		   "//#include \"" (ccide-file-name ".ih") "\"\n\n"
                   "// Custom includes\n\n"
		   "#define prefix_ inline\n"
                   "///////////////////////////////cci.p///////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
           (insert "\n\n///////////////////////////////cci.e///////////////////////////////////////\n"
		   "#undef prefix_"))

	  ((string-match "\\.ct$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " non-inline template implementation  */\n\n"
		   "//#include \"" (ccide-file-name ".ih") "\"\n\n"
                   "// Custom includes\n\n"
		   "#define prefix_\n"
                   "///////////////////////////////ct.p////////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
           (insert "\n\n///////////////////////////////ct.e////////////////////////////////////////\n"
		   "#undef prefix_"))

	  ((string-match "\\.cti$" (buffer-file-name))
	   (insert "/** \\file\n"
		   "    \\brief " (ccide-file-name) " inline template implementation */\n\n"
		   "//#include \"" (ccide-file-name ".ih") "\"\n\n"
                   "// Custom includes\n\n"
		   "#define prefix_ inline\n"
                   "///////////////////////////////cti.p///////////////////////////////////////\n\n")
	   (setq point (point))
	   (goto-char (point-max))
           (insert "\n\n///////////////////////////////cti.e///////////////////////////////////////\n"
		   "#undef prefix_"))

	  ((string-match "\\.dox$" (buffer-file-name))
	   (insert "/** \\mainpage\n\n    ")
	   (setq point (point))
	   (insert "\n */")
	   (setq add-file-vars '(( mode . flyspell)
				 ( mode . auto-fill))))

	  ((string-match "\\.java$" (buffer-file-name))
	   (setq mode "jde")
	   (setq point (point))
	   (goto-char (point-max)))

	  (t
	   (setq point (point))
	   (goto-char (point-max))))

    (insert "\n\n\n"
	    "// Local Variables:\n"
	    "// mode: " mode "\n")
    (loop for (var . value) in ccide-file-vars
	  do (insert "// " (symbol-name var) ": " (prin1-to-string value) "\n"))
    (loop for (var . value) in add-file-vars
	  do (insert "// " (symbol-name var) ": " (prin1-to-string value) "\n"))
    (insert "// End:\n")
    (if point
	(goto-char point))
    (if (equal mode "jde")
        (let ((package (file-name-directory (buffer-file-name))))
          (jdeap-initialize-setup)
          (if (not (equal jdeap-current-source-directory "."))
              (if (string-match 
                   (concat "^" (regexp-quote jdeap-current-source-directory))
                   package)
                  (progn
                    (setq package (substring package 
                                             (match-end 0)
                                             (1- (length package))))
                    (insert "package "
                            (string-replace "/" "." package t)
                            ";\n\n"))))
          (insert "class " (file-name-sans-extension
                            (file-name-nondirectory 
                             (buffer-file-name))) "\n{}")
          (beginning-of-line))))
  (run-hooks 'ccide-new-file-hooks))

(defun ccide-sync-file-variables ()
  "Syncronize file variables to the current value of ccide-file-vars"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (let ((case-fold-search t))
      (if (search-forward "Local Variables:" nil t)
	  (let (prefix suffix vars)
	    (skip-chars-forward " \t")
	    (or (eolp)
		(setq suffix (buffer-substring (point) (progn (end-of-line) (point)))))
	    (goto-char (match-beginning 0))
	    (or (bolp)
		(setq prefix (buffer-substring (point) (progn (beginning-of-line) (point)))))
	    (loop do (progn
		       (forward-line 1)
		       (if (and prefix (looking-at prefix))
			   (goto-char (match-end 0)))
		       (skip-chars-forward " \t"))
		  while (not (looking-at "end:"))
		  do (progn
		       (setq vars (cons (intern (buffer-substring 
						 (point) 
						 (progn (skip-chars-forward "^:\n") (point))))
					vars))))
	    (beginning-of-line)
	    (loop for (var . value) in ccide-file-vars
		  do (if (not (memq var vars))
			 (insert (or prefix "")
				 (symbol-name var) ": " (prin1-to-string value) 
				 (or suffix "") "\n"))))))))

(defun ccide-syncronize-includes ()
  "Syncronize include's in all other files"
  (interactive)
  (let (buffer-map)
    (loop for extension in ccide-special-extensions
	  for file-name = (ccide-file-name extension)
	  do (setq buffer-map
		   (cons (cons file-name
			       (or (find-buffer-visiting file-name)
				   (and (file-readable-p file-name)
					(find-file-noselect file-name))))
			 buffer-map)))
    (save-excursion
      (loop for buffer in buffer-map
	    if (cdr buffer)
	      do (progn 
		   (set-buffer (cdr buffer))
		   (save-excursion
		     (loop for include in buffer-map
			   do (progn 
				(goto-char (point-min))
				(while (re-search-forward 
					(concat "^\\(//\\)?#\\s-*include \""
						(regexp-quote (car include))
						"\"\\s-*$")
					nil t)
				  (goto-char (match-beginning 0))
				  (if (looking-at "//")
				      (if (cdr include)
					  (delete-char 2))
				    (if (not (cdr include))
					(insert "//")))
				  (forward-line 1))))))))))

(defun ccide-auto-decorate-new-files ()
  (if (= (point-min) (point-max))
      (let ((status (buffer-modified-p)))
	(ccide-file-comment)
	(set-buffer-modified-p status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class level

(defun ccide-class-comment ()
  "Add comment to start of current class definition"
  (interactive)
  (let ((class (c-get-class-at-point)))
    (if (not class)
	(error "No class found")
      (goto-char (or (aref (car class) 3)
		     (aref (car class) 1)))
      (if (save-excursion 
	    (forward-line -1)
	    (ccide-in-doxy-comment))
	  (progn
	    (search-backward "/**" nil t)
	    (forward-char 4))
	(let ((indent (make-string (current-indentation) ? )))
	  (insert "/** \\brief  ")
	  (save-excursion
	    (insert "\n"
		    indent " */\n"
		    indent)))))))

(defun ccide-gen-class (name &optional defns)
  "Generate class declaration template"
  (interactive (list (read-string (concat "Class name (default "
                                          (ccide-file-name)
                                          "): ")
                                  nil nil (ccide-file-name))))
  (insert "class " name)
  (indent-according-to-mode)
  (let ((in (make-string c-basic-offset ? ))
	(ofs (make-string (current-indentation) ? )))
    (save-excursion
      (beginning-of-line)
      (open-line 1)
      (insert ofs "/** \\brief\n"
              ofs "  */"))
    (insert "\n" ofs)
    (save-excursion
      (insert "{\n"
	      ofs "public:\n"
              ofs in "///////////////////////////////////////////////////////////////////////////\n"
              ofs in "// Types\n\n"
              ofs in "///////////////////////////////////////////////////////////////////////////\n"
              ofs in "///\\name Structors and default members\n"
              ofs in "///@{\n\n"
	      ofs in "// default default constructor\n"
	      ofs in "// default copy constructor\n"
	      ofs in "// default copy assignment\n"
	      ofs in "// default destructor\n\n"
              ofs in "// no conversion constructors\n\n"
              ofs in "///@}\n"
              ofs in "///////////////////////////////////////////////////////////////////////////\n"
              ofs in "///\\name Accessors\n"
              ofs in "///@{\n\n"
              ofs in "///@}\n"
              ofs in "///////////////////////////////////////////////////////////////////////////\n"
              ofs in "///\\name Mutators\n"
              ofs in "///@{\n\n"
              ofs in "///@}\n\n")
      (loop for defn in defns
	    do (insert ofs in defn ";\n"))
      (if defns
	  (insert "\n"))
      (insert ofs "protected:\n\n"
	      ofs "private:\n\n"
	      ofs "};\n"))))

(defun ccide-gen-class-defaults ()
  "Generate signatures of the default functions: default constructor,
copy constructor, assignment operator and destructor."
  (indent-according-to-mode)
  (let* ((name (c-scope-name (aref (car (c-get-class-at-point)) 1)))
	 (in (make-string c-basic-offset ? ))
	 (ofs (make-string (current-indentation) ? ))
	 (tspec (if ccide-gen-throw (concat "\n" ofs in "throw_(());\n") ";\n"))
	 (colon 0))
    (while (string-match "::" name colon)
      (setq colon (match-end 0)))
    (setq name (substring name colon))
    (beginning-of-line)
    (delete-horizontal-space)
    (loop with exit = nil
	  do (message (concat "1-dflt constr, 2-destr, "
			      "3-copy constr, 4-copy assmnt, "
			      "c-all copy, d-all dflt, RET-all/done: "))
	  for ch = (read-event)
	  for first = t then nil
	  do (cond ((eq ch 'return)
		    (if first
			(insert ofs name "()" 
				tspec
				ofs name "(const " name "& other)" 
				tspec
				ofs "~" name "();\n"
				ofs name "& operator=(const " name "& other)"
				tspec))
		    (setq exit t))
		   ((eq ch ?1)
		    (insert ofs name "()" 
			    tspec))
		   ((eq ch ?2)
		    (insert ofs "~" name "();\n"))
		   ((eq ch ?3)
		    (insert ofs name "(const " name "& other)" 
			    tspec))
		   ((eq ch ?4)
		    (insert ofs name "& operator=(const " name "& other)"
			    tspec))
		   ((eq ch ?c)
		    (insert ofs name "(const " name "& other)" 
			    tspec
			    ofs name "& operator=(const " name "& other)"
			    tspec))
		   ((eq ch ?d)
		    (insert ofs name "()" 
			    tspec
			    ofs "~" name "();\n"))
		   (t (setq unread-command-events (cons ch unread-command-events))
		      (setq exit t)))
	  while (not exit))))

(defun ccide-gen-class-defaults-impl ()
  "Generate default implementations for class default functions"
  (interactive)
  (let ((defn (c-build-default-funcions-impl)))
    (kill-new (cadr defn))
    (message (concat (car defn) " default members"))))

(defun ccide-set-class-defaults-comment (word)
  (save-excursion
    (back-to-indentation)
    (if (not (looking-at ccide-class-defaults-word))
	(message "Not at class defaults commnet")
      (replace-match word t t nil 1))))

(defmacro ccide-build-class-defaults-f (sym)
  (let ((fn (intern (concat "ccide-set-class-defaults-" 
			    (symbol-name sym)))))
    `(defun ,fn ()
       (interactive)
       (ccide-set-class-defaults-comment ,(symbol-name sym)))))

(ccide-build-class-defaults-f no)
(ccide-build-class-defaults-f default)
(ccide-build-class-defaults-f my)
(ccide-build-class-defaults-f protected)
(ccide-build-class-defaults-f private)
(ccide-build-class-defaults-f disabled)

(defun ccide-gen-struct-constructors ()
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)
    (let* ((scope (c-get-block-scope))
	   (class (c-parse-class scope))
	   (variables (c-get-variable-members-with-type class))
	   (name (c-scope-name (aref (car (last scope)) 1)))
	   (in (make-string (current-indentation) ? ))
	   (inin (make-string (+ (current-indentation) c-basic-offset) ? )))
      (insert name "()\n" inin ": ")
      (indent-according-to-mode)
      (loop for var in variables
	    for first = t then nil
	    if (not first) do (insert ", ")
	    do (insert (car var) "()"))
      (insert "\n" in "{}\n"
	      in name "(")
      (loop for var in variables
	    for first = t then nil
	    if (not first) do (insert ", ")
	    do (insert (cdr var) " " (car var) "_"))
      (insert ")\n" inin ": ")
      (loop for var in variables
	    for first = t then nil
	    if (not first) do (insert ", ")
	    do (insert (car var) "(" (car var) "_)"))
      (insert "\n" in "{}"))))

(defun ccide-class-impl-comment ()
  "Get implementation comment for current class"
  (interactive)
  (let* ((scope (c-get-block-scope))
         (name (c-get-full-prefix scope)))
    (kill-new (concat (make-string 75 ?/) "\n"
                      "// " name "\n\n"
                      "// protected\n\n"
                      "// private\n\n"))
    (message name)))

(defun ccide-gen-exception (class &optional description)
  (interactive "sException name: \nsDescription (defaults to full class name): ")
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode)
  (save-excursion
    (let ((in (make-string c-basic-offset ? ))
	  (ofs (make-string (current-indentation) ? ))
	  (prefix (c-get-full-prefix (c-get-block-scope)))
	  p)
      (insert "struct " class " : public std::exception\n"
	      ofs "{ virtual char const * what() const throw() ")
      (setq p (point))
      (insert "{ return \"" 
	      (if (and description (> (length description) 0))
		  description
		(concat prefix "::" class))
	      "\"; } };")
      (if (> (current-column) fill-column)
	  (save-excursion
	    (goto-char p)
	    (insert "\n" ofs in in))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function/method level

(defun ccide-function-comment ()
  "Add comment for current function"
  (interactive)
  (if (memq (c-in-literal) '(c c++))
      ; Assume, we are in the functions comment ...
      (progn
	(c-forward-out-of-comment)
	(c-backward-syntactic-ws)
	(c-backward-sexp))
    (beginning-of-line))
  (c-beginning-of-defun-or-decl)
  (let ((defun (c-get-defun-state))
	(indent (make-string comment-column ? ))
	place)
    (goto-char (or (aref defun 7) (car (aref defun 6))))
    (c-backward-syntactic-ws)
    (if (looking-at "[ \t\n\r]*///<")
	(progn
	  (delete-region (point) (progn (skip-chars-forward " \t\n\r") (point)))
	  (if (> (current-column) comment-column)
	      (insert "\n"))
	  (indent-to-column comment-column)
	  (search-forward "*/")
	  (forward-char -2))
      (if (> (current-column) comment-column)
	  (insert "\n"))
      (indent-to-column comment-column)
      (insert "///< ")
      (setq place (point))
      (insert "\n" 
	      indent "/**< ")
      (insert             "\\pre \n"
			  indent "     \\post */")
      (save-excursion
	(goto-char (car (aref defun 2)))
	(setq defun (c-get-defun-state)))
      (forward-char -2))
    (ccide-function-comment-adjust defun (concat indent "     "))
    (if place (goto-char place))))

(defun ccide-function-comment-grab-args ()
  (let ((limit (save-excursion
		 (search-backward "/**" nil t)
		 (point)))
	(end  (point))
	begin start args argend)
    (setq argend end)
    (while (or (search-backward "\\param" limit t)
	       (search-backward "\\return" limit t)))
    (beginning-of-line)
    (setq start (point))
    (setq begin start)
    (while (search-forward "\\param" argend t)
      (or (search-forward "\\param" argend t)
	  (search-forward "\\return" argend t)
	  (goto-char argend))
      (beginning-of-line)
      (setq args (cons (ccide-function-comment-parse-arg start (point))
		       args))
      (setq start (point)))
    (prog1
	(if (not (search-forward "\return" argend t))
	    (cons nil args)
	  (beginning-of-line)
	  (cons (buffer-substring (point) argend) args))
      (delete-region begin end))))

(defun ccide-function-comment-parse-arg (start end)
  (save-excursion
    (goto-char start)
    (re-search-forward "\\\\param\\(\\[[^]]*\\]\\)?\\s-*\\(\\S-*\\)" end t)
    (cons (match-string 2) 
	  (cons (buffer-substring start (match-beginning 2))
		(buffer-substring (match-end 2) end)))))
  
(defun ccide-function-comment-get-throws (defun)
  (if (aref defun 4)
      (save-excursion
	(goto-char (car (aref defun 4)))
	(if (re-search-forward "\\(throw_\\|throw\\)((?\\s-*\\([^()]*\\))?)" 
			       (cdr (aref defun 4)) t)
	    (let ((spec (match-string 2)))
	      (if (> (length spec) 0)
		  spec))))))

(defun ccide-function-comment-adjust (defun indent)
  (insert "\n")
  (let* ((defargs (mapcar (function (lambda (x)
				      (c-get-template-argument-name (car x) (cdr x))))
			  (aref defun 3)))
	 (defret (and (aref defun 1)
		      (not (string-match (concat "^\\("
						 c-special-key 
						 "\\s-*\\)*\\s-*void$")
					 (buffer-substring (car (aref defun 1))
							   (cdr (aref defun 1)))))))
	 (throws (ccide-function-comment-get-throws defun))
	 (xargs (ccide-function-comment-grab-args))
	 (docargs (cdr xargs))
	 (docret (car xargs))
	 (def-in-doc (loop for defarg in defargs always (assoc defarg docargs)))
	 (doc-in-def (loop for docarg in docargs always (member (car docarg) defargs)))
	 (size-eq (= (length defargs) (length docargs))))
    ;; We differentiate four types changes
    ;;  - new arguments
    ;;  - removed arguments
    ;;  - reordered arguments
    ;;  - renamed arguments
    ;; 
    ;; If the change cannot be described by one of the above, it has
    ;; to be resolved manually
    (if throws
	(insert indent "\\throws " throws "\n"))
    (cond (doc-in-def
	   ;; reordered arguments or new arguments (or no change)
	   (loop for defarg in defargs
		 for docarg = (assoc defarg docargs)
		 do (if docarg
			(insert (cadr docarg) (car docarg) (cddr docarg))
		      (insert indent "\\param " defarg " \n"))))
	  (size-eq ; and (not doc-in-def)
	   ;; renamed arguments
	   (loop for defarg in defargs
		 for docarg in docargs
		 do (insert (cadr docarg) defarg (cddr docarg))))
	  (def-in-doc
	    ;; removed arguments
	    (loop for defarg in defargs
		  for docarg = (assoc defarg docargs)
		  do (insert (cadr docarg) (car docarg) (cddr docarg))))
	  (t (error "Arg change too complex. Resolve manualy.")))
    ;; return value is simple
    (if defret
	(if docret
	    (insert docret)
	  (insert indent "\\return \n"))))
  (delete-char -1)
  (delete-horizontal-space)
  (insert " "))

(defun ccide-grab-prototype (&optional prefix)
  "Grab prototype of function defined or declared at point. Prefix
arg, if given, specifies the kind of prefix (inline, static, ...) to use."
  (interactive "P")
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let* ((prfx (or (and prefix (nth (prefix-numeric-value prefix) c-user-prefixes))
		     ccide-default-prefix))
	   (defn (c-build-defun prfx)))
      (kill-new (concat (cadr defn) "\n{}\n"))
      (message (concat (or prfx "")
		       (if prfx " " "")
		       (car defn))))))

(defun ccide-reformat-defun ()
  "Reformat the defn of the current defun."
  (interactive)
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let ((defn (c-build-defun nil t)))
      (delete-region (or (caar (aref (caddr defn) 0))
			 (car (aref (caddr defn) 1))
			 (car (aref (caddr defn) 2)))
		     (or (car (aref (caddr defn) 6))
			 (aref (caddr defn) 7)))
      (insert (cadr defn) "\n"))))

(defun ccide-replace-defun ()
  "Replace the function header with the one on the top of the kill
ring (presumably placed there using c++-grab-prototype)."
  (interactive)
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let ((parse (c-parse-defun)))
      (delete-region (or (aref parse 0)
			 (aref parse 1)
			 (aref parse 2))
		     (or (aref parse 5)
			 (aref parse 6)
			 (aref parse 7)))
      (yank)
      (delete-char -3))))

(defun ccide-prefix-defun-type-with-class (&optional strip)
  "If a non-keyword type symbol is found prefixing the current defun,
it will be prefixed with the current class prefix."
  (interactive "p")
  (save-excursion
    (c-beginning-of-defun-or-decl)
    (let* ((parse (c-parse-defun))
	   (prefix (c-scope-name (aref parse 2) (+ (or strip 0) 0))))
      (goto-char (aref parse 1))
      (while (and (or (looking-at c-any-key)
		      (looking-at c-user-prefix-re)
		      (not (c-at-symbol-p)))
		  (< (point) (aref parse 2))
		  (not (eobp)))
	(c-forward-token-1)
	(c-forward-syntactic-ws))
      (if (and (c-at-symbol-p)
	       (< (point) (aref parse 2))
	       (not (looking-at (regexp-quote prefix))))
	  (let ((pos (string-match "<" prefix)))
	    (if pos
		(insert "typename "))
	    (if (and pos (looking-at (concat (substring prefix 0 pos)
					     "\\b[^_]")))
		(progn
		  (goto-char (match-end 0))
		  (c-backward-syntactic-ws)
		  (insert (substring prefix pos)))
	      (insert prefix "::"))
	    (ccide-reformat-defun))))))

(defun ccide-prefix-defun-type-with-namespace (&optional strip)
  (interactive "p")
  (ccide-prefix-defun-type-with-class (+ (or strip 0) 1)))

(defun ccide-insert-defun-prefix (&optional strip)
  "Insert the current defun prefix at point."
  (interactive "p")
  (let* ((parse (c-parse-defun))
	 (prefix (c-scope-name (aref parse 2) (+ (or strip 0) 0))))
    (insert prefix "::")))

(defun ccide-kill-inline-decl (defn)
  (save-excursion
    (if (aref (caddr defn) 6)
	(progn
	  (goto-char (cdr (aref (caddr defn) 6)))
	  (let ((end-mark (point-marker)))
	    (goto-char (car (aref (caddr defn) 6)))
	    (indent-rigidly (point) end-mark
			    (- (current-column)))
	    (prog1
		(concat (cadr defn)
			"\n"
			(buffer-substring-no-properties (point) end-mark)
			"\n")
	      (when (aref (caddr defn) 5)
		(goto-char (caar (aref (caddr defn) 5)))
		(c-backward-syntactic-ws)
		(skip-chars-backward ":"))
	      (c-backward-syntactic-ws)
	      (delete-region (point) end-mark)
	      (insert ";"))))
      (concat (cadr defn) "\n{}\n"))))

(defun ccide-grab-inline-decl ()
  "Grab the inline decl at point at turn it into an out-of-line inline
declaration at the top of the kill ring."
  (interactive)
  (let ((defn (c-build-defun (or ccide-default-prefix "inline"))))
    (kill-new (ccide-kill-inline-decl defn))
    (message (concat (or ccide-default-prefix "indline") 
		     " " 
		     (car defn)))))

(defun ccide-grab-all-inlines ()
  "Grab all inline decls in the current class"
  (interactive)
  (let ((class (c-parse-class (c-get-block-scope)))
	defns)
    (when class
      (loop for method in (nreverse (aref class 4))
	    do (when (eq (car method) 'method)
		 (let ((defn (save-excursion
			       (goto-char (cdr method))
			       (c-build-defun (or ccide-default-prefix "inline")))))
		   (if (aref (caddr defn) 6)
		       (setq defns (nconc defns (list (ccide-kill-inline-decl defn))))))))
      (kill-new (loop for defn in (nreverse defns)
		      for next = nil then t
		      if next concat "\n";
		      concat defn))
      (message (format "%d inlines grabed to kill ring" (length defns))))))
		       

(defun ccide-grab-create-constructor ()
  (interactive)
  (let ((defn (c-build-create-constructor)))
    (kill-new (cadr defn))
    (message (car defn))))

(defun ccide-grab-create-constructor-impl (&optional prefix)
  (interactive "P")
  (let* ((prfx (or (and prefix (nth (prefix-numeric-value prefix) c-user-prefixes))
		   ccide-default-prefix))
	 (defn (c-build-create-constructor-impl prfx)))
    (kill-new (cadr defn))
    (message (concat (or prfx "")
		     (if prfx " " "")
		     (car defn)))))

(defun ccide-find-implementation (&optional other-window)
  "Find implementation of method declared at point."
  (interactive "P")
  (let* ((state (c-get-defun-state))
	 (name (c-defun-short-name state))
	 (scoped-name (c-defun-full-name state))
	 (args (ccide-implementation-args state))
	 (targs (ccide-implementation-template-args state))
         rv fallback)

    (loop for ext in ccide-implementation-extensions
          for filename = (ccide-file-name ext)
          while (not rv)
          do (progn
               (let ((buf (or (find-buffer-visiting filename)
                                  (and (file-readable-p filename)
                                       (find-file-noselect filename)))))
                 (when buf
                   (let ((found (save-excursion
                                  (set-buffer buf)
                                  (ccide-find-implementation-1 name scoped-name args targs
							       (not (aref state 6))
                                                               (car (aref state 2))))))
                     (if found
                         (if (cdr found)
                             (setq rv (cons buf found))
                           (if (not fallback) (setq fallback (cons buf found))))))))))
    (if (not rv) (setq rv fallback))
    (if rv
        (let* ((buf (car rv))
               (pos (cadr rv))
               (win (get-buffer-window buf)))
          (if win 
              (select-window win)
            (if other-window
                (switch-to-buffer-other-window buf)
              (switch-to-buffer buf)))
          (goto-char pos)
          (forward-char -1)
          (c-beginning-of-defun-or-decl))
      (message (concat "Implementation of " scoped-name " not found.")))))

(defun ccide-implementation-args (state)
  (string-replace "[ \t\n\r]+" ""
		  (loop for (start . end) in (aref state 3)
			for sep = "" then ","
			concat sep
			concat (buffer-substring-no-properties 
				start (save-excursion
					(goto-char start)
					(if (search-forward "=" end 'move) (forward-char -1))
					(point))))
		  
		  t))

(defun ccide-implementation-template-args (state)
  (and (aref state 0)
       (string-replace "[ \t\n\r]+" ""
		       (loop for (start . end) in   (save-excursion
						      (goto-char (caar (last (aref state 0))))
						      (c-parse-template-declaration))
			     for sep = "" then ","
			     concat sep
			     concat (buffer-substring-no-properties 
				     start (save-excursion
					     (goto-char start)
					     (if (search-forward "=" end 'move) (forward-char -1))
					     (point))))
		       t)))

(defun ccide-find-implementation-1 (name scoped-name args targs with-body skip-def)
  ;; Within the current buffer, search for all implementations of the
  ;; given function. The rv is a list of conses. The car holds the
  ;; buffer position of the implementation, the cdr is t if the name,
  ;; scoped-name and args are matched, otherwise the args did not match.
  (save-excursion
    (goto-char (point-min))
    (let ((re (concat (if (eq (char-syntax (aref name 0)) ?w) "\\<" "")
		      (regexp-quote name)
		      (if (eq (char-syntax (aref name (1- (length name)))) ?w) "\\>" "")))
	  fallback rv check-state)
      (while (and (not rv) (re-search-forward re nil t))
        (if (and (c-at-toplevel-p) 
                 (not (c-in-literal))
                 (setq check-state (condition-case nil (c-get-defun-state) (error nil)))
                 (not (= (car (aref check-state 2)) skip-def)))
            (if (string= scoped-name (c-defun-full-name check-state))
                (if (and (if with-body (aref check-state 6) (not (aref check-state 6)))
		         (string= args (ccide-implementation-args check-state))
			 (string= targs (ccide-implementation-template-args check-state)))
                    (setq rv (cons (point) t))
                  (if (not fallback) 
                      (setq fallback (cons (point) nil)))))))
      (or rv fallback))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable/type level
  
(defun ccide-variable-comment ()
  "Add a comment to current variable declaration."
  (interactive)
  (c-forward-out-of-comment)
  (c-forward-syntactic-ws)
  (while (not (looking-at ";"))
    (c-forward-sexp)
    (c-forward-syntactic-ws))
  (forward-char 1)
  (if (> (current-column) comment-column)
      (insert "\n" (make-string comment-column ? ) "///< ")
    (indent-to-column comment-column)
    (insert "///< ")))

(defun ccide-grab-access-fn ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at (concat c++-simple-type-regexp "[ \t\n\r][a-zA-Z0-9_]+[ \t\n\r]*;"))
	(let ((vardef (match-string 0))
	      (in (make-string c-basic-offset ? ))
	      type reftype varname fnname argname ws)
	  (forward-line -1)
	  (back-to-indentation)
	  (string-match "^[ \t\n\r]*\\(.*\\)[ \t\n\r]\\([a-zA-Z0-9_]+\\)[ \t\n\r]*;$"
			vardef)
	  (setq varname (match-string 2 vardef)
		type (match-string 1 vardef)
		ws (substring vardef 0 (match-beginning 1)))
	  (if (string-match "_$" varname)
	      (setq fnname (string-replace "_$" "" varname)
		    argname (concat "a_" fnname))
	    (setq fnname (concat "q_" varname)
		  argname (concat "a_" varname)))
	  (if (string-match "^[ \t\n\r]*" type)
	      (setq type (substring type (match-end 0))))
	  (if (string-match "^[A-Z]" type)
	      (setq reftype (concat type " const &"))
	    (setq reftype type))
	  (kill-new (concat ws type " " fnname "(" reftype " " argname ")\n"
			    ws in "{\n"
			    ws in in type " old" varname " = " varname ";\n"
			    ws in in varname " = " argname ";\n"
			    ws in in "return old" varname ";\n"
			    ws in "}\n\n"
			    ws reftype " " fnname "() const\n"
			    ws in "{ return " varname "; }\n"))
	  
	  (message varname))
      (message "No variable found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doxy comment support functions

(defun ccide-special-indent-function ()
  "Function to indent doxy comments correctly"
  (let ((indent (ccide-in-doxy-comment)))
    (if indent
	(let ((lim (save-excursion
		     (back-to-indentation)
		     (c-literal-limits)))
	      (pos (- (point-max) (point))))
	  (save-excursion
	    (back-to-indentation)
	    (if (looking-at "*/")
		(incf indent -3)
	      (let ((para (or (save-excursion (re-search-backward "^\\s-*$" (car lim) t))
			      (car lim))))
		(if (and (not (looking-at ccide-doxy-tag-re))
			 (re-search-backward (concat "^\\s-*"
						     ccide-doxy-tag-re)
					     para t))
		    (incf indent 4)))))
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (back-to-indentation) (point)))
	  (indent-to indent)
	  (if (> (- (point-max) pos) (point))
	      (goto-char (- (point-max) pos)))))))
  
(defun ccide-fill-function ()
  "auto-fill function for doxy comments"
  (if (do-auto-fill)
      (if (not fill-prefix)
	  (indent-according-to-mode))))

(defun ccide-hide-all-doxy-comments ()
  "Hide all doxy comments"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*/\\*\\*" nil t)
      (beginning-of-line)
      (forward-line -1)
      (if (not (looking-at "\\s-*$"))
	  (forward-line 1))
      (forward-char -1)
      (let ((start (point)))
	(if (re-search-forward "\\*/" nil t)
	    (progn
	      (if (looking-at "\\s-*\n")
		  (forward-line 1))
              (forward-char -1)
              (let ((overlay (make-overlay start (point))))
                (overlay-put overlay 'intangible 'hs)
                (overlay-put overlay 'invisible 'hs)))))))
  (message "Done."))

(defun ccide-show-all-comments ()
  "Show all comments"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (goto-char (next-overlay-change (point)))
      (loop for overlay in (overlays-at (point))
            if (eq (overlay-get overlay 'invisible) 'hs)
            do (delete-overlay overlay))))
  (message "Done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORBA support (omniORB2)

(defun ccide-get-corba-headers ()
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#include\\s-*[\"<]\\([^\">]*\\)\\.hh[\">]" nil t)
	(setq files (cons (ccide-match-string 1) files)))
      (nreverse files))))

(defun ccide-corba-maybe-build-hh (file)
  (let ((skel (ccide-file-name ".hh" file ccide-corba-skel-dir))
	(idl (ccide-file-name ".idl" file ccide-corba-idl-dir)))
    (if (and (file-readable-p idl)
	     (or (not (file-readable-p skel))
		 (file-newer-than-file-p idl skel)))
	(let ((buffer (find-buffer-visiting (ccide-file-name ".hh" file))))
          (if buffer
              (kill-buffer buffer))
	  (message "Please wait ... building %s" (ccide-file-name ".hh" file))
	  (if (ccide-shell-command (concat "cd " 
					   (real-path-name ccide-corba-skel-dir) 
					   " && " 
					   ccide-corba-idl-command 
					   (if (> (length ccide-corba-idl-dir) 0)
					       (concat " -I" ccide-corba-idl-dir))
					   " " 
					   idl))
	      ()
	    (display-buffer (get-buffer-create "*ccide shell command*"))
	    (error "Generation of %s failed" (ccide-file-name ".hh")))))
    (if (not (file-readable-p skel))
	(error "No file %s or %s" 
	       (ccide-file-name ".hh" file) (ccide-file-name ".idl" file)))))

(defun ccide-corba-list-skeletons-1 (hh-file)
  (ccide-corba-maybe-build-hh hh-file)
  (let ((hh-buf (find-file-noselect (ccide-file-name ".hh" hh-file)))
	skels)
    (save-excursion
      (set-buffer hh-buf)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\s-*class\\s-+_sk_\\([a-zA-Z0-9_]+\\)\\s-+:"
				  nil t)
	  (setq skels (cons (ccide-match-string 1) skels)))))
    (mapcar (function (lambda (x) (cons x hh-file)))
	    (sort skels 'string-lessp))))

(defun ccide-corba-list-skeletons ()
  (let ((files (ccide-get-corba-headers)))
    (loop for file in files
	  append (ccide-corba-list-skeletons-1 file))))

(defun ccide-gen-corba-impl (class)
  (interactive (list (completing-read "Class name of skeleton: "
				      (ccide-corba-list-skeletons)
				      nil t)))
  (let* ((skels (ccide-corba-list-skeletons))
	 (hh-file (ccide-file-name ".hh" (cdr (assoc class skels))
				   ccide-corba-skel-dir))
	 (hh-buf (find-file-noselect (ccide-file-name ".hh" hh-file
						      ccide-corba-skel-dir))))
    (ccide-gen-class (concat class "_i"))
    (insert (make-string c-basic-offset ? ) ": public virtual _sk_" class "\n")
    (save-excursion
      (search-forward "protected:" nil t)
      (forward-line -1)
      (ccide-gen-corba-impl-methods)
      (insert "\n"))))

(defun ccide-get-corba-defns (hh-file class)
  (let ((hh-buf (find-file-noselect hh-file))
	defns)
    (save-excursion
      (set-buffer hh-buf)
      (save-excursion
	(goto-char (point-min))
	(if (not (re-search-forward (concat "^\\s-*class\\s-+_sk_" class "\\s-+:")
				    nil t))
	    (error "CORBA skeleton class not found.")
	  (search-forward "{")
	  (forward-char -1)
	  (let ((end (save-excursion (forward-sexp) (point))))
	    (while (and (< (point) end)
			(< (forward-line 1) 1))
	      (if (looking-at "\\s-+virtual\\s-+\\(.*)\\)\\s-*=\\s-*0;\\s-*$")
		  (setq defns (cons (match-string 1) defns))))))))
    (nreverse defns)))

(defun ccide-gen-corba-impl-methods ()
  (interactive)
  (let* ((class (c-get-class-at-point))
	 (point (point)))
    (if (not class)
	(error "No class at point."))
    (save-excursion
      (goto-char (aref (car class) 1))
      (if (not (re-search-forward ":\\s-*public\\s-*virtual\\s-*_sk_\\([^ \t\n\r{},:]*\\)"
				  nil t))
	  (error "No CORBA impl at point."))
      (let* ((name (ccide-match-string 1))
	     (skels (ccide-corba-list-skeletons))
	     (hh-file (ccide-file-name ".hh" (cdr (assoc name skels))
				       ccide-corba-skel-dir))
	     (defns (ccide-get-corba-defns hh-file name))
	     end)
	(goto-char (aref (car class) 2))
	(save-excursion
	  (c-forward-sexp)
	  (setq end (point)))
	(if (re-search-forward "^\\s-*// CORBA$" end t)
	    (let ((start (match-beginning 0)))
	      (if (re-search-forward "^\\s-*// END-CORBA$" end t)
                  (let ((eend (match-end 0)))
                    (goto-char start)
                    (forward-line 1)
                    (if (re-search-forward "/\\*\\|//" (match-beginning 0) t)
                        (if (y-or-n-p "Remove CORBA Funktion comments? (y/n)")
                            (delete-region start (1+ eend))
                          (goto-char eend)
                          (beginning-of-line)
                          (delete-region (point) (progn 
                                                   (end-of-line)
                                                   (1+ (point))))
                          (save-excursion
                            (goto-char start)
                            (delete-region (point) (progn 
                                                     (end-of-line)
                                                     (1+ (point)))))
                          (insert "\n"))
                      (delete-region start (1+ eend))))))
	  (goto-char point))
	(indent-according-to-mode)
	(insert "// CORBA\n")
	(loop for defn in defns
	      do (progn
		   (save-excursion (insert defn ";"))
		   (indent-according-to-mode)
                   (let ((start (point)) end)
                     (end-of-line)
                     (setq end (point))
                     (goto-char start)
                     (while (re-search-forward "\\s-+" end t)
                       (replace-match " ")
                       (setq end (- end (- (match-end 0) (match-beginning 0) 1))))
                     (end-of-line)
                     (loop with done = nil
                           while (> (current-column) c-max-def-column)
                           do (while (and (> (current-column) c-max-def-column)
                                          (search-backward "," start t)))
                           do (if (looking-at ",")
                                  (progn
                                    (forward-char 1)
                                    (insert "\n")
                                    (open-line 1)
                                    (indent-according-to-mode)
                                    (delete-char 2)
                                    (setq start (point))
                                    (end-of-line))
                                (setq done t))
                           while (not done)))
                   (insert "\n")))
	(indent-according-to-mode)
	(insert "// END-CORBA\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template support

(defun ccide-scan-mantemps ()
  "Scan *compilation* buffer for errors and generate manual template
instatiations at point."
  (interactive)
  (save-excursion
    (set-buffer "*compilation*")
    (goto-char (point-min)))
  (save-excursion
    (set-buffer (get-buffer-create "*mantemps*"))
    (erase-buffer)
    (loop for temp = (ccide-get-mantemp)
	  while temp
	  do (insert temp "\n"))
    (mantemp-make-mantemps-buffer)
    (goto-char (point-min))
    (while (progn
	     (ccide-fix-mantemp)
	     (< (forward-line 1) 1))))
  (insert-buffer-substring "*mantemps*"))

(defun ccide-get-mantemp ()
  (save-excursion
    (set-buffer "*compilation*")
    (if (search-forward "undefined reference to `" nil t)
	(let ((start (point)))
	  (end-of-line)
	  (search-backward "'" nil t)
	  (buffer-substring start (point))))))

(defun ccide-fix-mantemp ()
  (let ((end (save-excursion
	       (end-of-line) (point))))
    (if (and (save-excursion (search-forward "(" end t))
	     (search-forward " class" end t))
	(progn
	  (forward-char -6)
	  (delete-char 6)))))
      
(provide 'cc-ide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other stuff

(defun ccide-open-compilation-frame ()
  (interactive)
  (let ((c-frame (selected-frame))
        (compilation-frame (make-frame '((minibuffer . nil) 
                                         (unsplittable . t) 
                                         (menu-bar-lines . 0) 
                                         (top . -87) 
                                         (left . 36) 
                                         (width . 169) 
                                         (height . 9)))))
    (select-frame compilation-frame)
    (switch-to-buffer "*compilation*")
    (set-window-dedicated-p (selected-window) t)))

(defun ccide-compile (command)
  (delete-other-windows)
  (split-window-horizontally)
  (compile command)
  (save-excursion
    (set-buffer "*compilation*")
    (let ((point (point-max)))
      (goto-char point)
      (loop for window in (get-buffer-window-list "*compilation*" nil t)
            do (set-window-point window point)))))

(defun ccide-compile-compile ()
  (interactive)
  (ccide-compile (concat "make -k " ccide-compile-opts)))

(defun ccide-compile-clean ()
  (interactive)
  (ccide-compile (concat "make -k " ccide-compile-opts " clean")))

(defun ccide-compile-cleandepends ()
  (interactive)
  (ccide-compile (concat "make -k " ccide-compile-opts " cleandepends")))

(defun ccide-compile-kill ()
  (interactive)
  (set-buffer "*compilation*")
  (kill-compilation))

(defun ccide-hide-compilation ()
  (interactive)
  (let ((active (selected-window)))
    (unwind-protect
        (loop for window in (get-buffer-window-list "*compilation*")
              do (progn (select-window window)
                        (switch-to-buffer (other-buffer "*compilation*"))))
      (select-window active))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap and installation

(defun ccide-bind-keys (prefix map)
  (loop for binding in ccide-bindings
	do (apply 'vcmd-define-key
		  map
		  (concat prefix (car binding))
		  (cadr binding)
		  "IDE"
		  (cddr binding))))

(defun ccide-install-it ()
  (save-excursion
    (hs-minor-mode 1)
    (hs-show-all))
  (local-unset-key "\C-c;")
  (local-unset-key [menu-bar IDE])
  (ccide-bind-keys "\C-c;" (current-local-map))
  (local-set-key "\C-cC" 'ccide-hide-all-doxy-comments)
  (local-set-key "\C-cS" 'ccide-show-all-comments)
  (set (make-local-variable 'auto-fill-function) 'ccide-fill-function)
  (set (make-local-variable 'paragraph-start) (concat "[ \t\f]*$\\|[ \t\f]*" ccide-doxy-tag-re))
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (auto-fill-mode -1)
  (ccide-project-load-config)
  (ccide-directory-load-config)
  (ccide-auto-decorate-new-files))

(defun ccide-project-load-config ()
  (let ((conf (ccide-project-search-upwards "project.el" (file-name-directory (buffer-file-name)))))
    (when conf
      (set (make-local-variable 'ccide-project-root) (file-name-directory conf))
      (load-file conf))))

(defun ccide-project-search-upwards (file &optional dir)
  "Search for FILE in all directories starting at DIR and going up the directory hierarchy.
DIR defaults to ccide-project-root"
  (let (conf last-dir)
    (setq dir (expand-file-name "x" (or dir ccide-project-root)))
    (while (and (not (string= (setq last-dir dir 
				    dir (directory-file-name (file-name-directory dir))) last-dir))
		(setq conf (expand-file-name file dir))
		(not (file-readable-p conf))))
    (and (file-readable-p conf) conf)))

(defun ccide-directory-load-config ()
  (if (file-readable-p ".dir.el")
      (load-file ".dir.el")))

(add-hook 'c-mode-hook 'ccide-install-it)
(add-hook 'c++-mode-hook 'ccide-install-it)
(add-hook 'c-special-indent-hook 'ccide-special-indent-function)

(loop for extension in ccide-special-extensions
      for re = (concat (regexp-quote extension) "$")
      if (not (assoc re auto-mode-alist))
        do (setq auto-mode-alist (append auto-mode-alist
					 (list (cons re 'c++-mode)))))

(defadvice c-indent-line (after c-indent-less compile disable) ;activate
  ;; new indent function for c-mode: do standard indentation first. If line
  ;; is to long using standard indentation, just indent by c-basic-indentation.
  (let ((cc (save-excursion (end-of-line) (current-column)))
	indent)
    (if (> cc  85)
	(let ((pos (- (point-max) (point))))
	  (beginning-of-line)
	  (let ((point (point))
		(line (1+ (count-lines 1 (point))))
		indent)
	    (c-beginning-of-statement-2)
	    (if (and (not (c-crosses-statement-barrier-p (point) point))
		     (not (eq (+ (count-lines 1 (point))
				 (if (bolp) 1 0))
			      line)))
		(progn
		  (setq indent (+ (current-indentation) c-basic-offset))
		  (goto-char point)
		  (if (< indent (current-indentation))
		      (progn
			(setq ad-return-value
			      (+ ad-return-value
				 (- (current-indentation) indent)))
			(delete-region (c-point 'bol) (c-point 'boi))
			(indent-to indent))))))
	  (if (< (point) (c-point 'boi))
	      (back-to-indentation)
	    (if (> (- (point-max) pos) (point))
		(goto-char (- (point-max) pos))))))))



;;; Local Variables:
;;; elisp-project-autoload-file-name: "cc-autoload.el"
;;; End:
