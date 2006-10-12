; Problem with this RE: a) If the line before the function declaration
; is a preprocessor directive (like a #define) ending in one or more
; c++ type keywords they will get included into the type spec.  b) if
; you define a symbol like #define _const const the expression won't
; work at all

; Another possible regexp for c++-simple-type-regexp is "^.+". This
; will make any text before the name of the function but in the same
; line be the type of the function. Problem: Type cannot span multiple
; lines.

(defvar c++-prefixes '("__inline__" "static"))

(defconst c++-simple-type-regexp "\\(\\([ \t\n\r]+\\(inline\\|explicit\\|unsigned\\|signed\\|short\\|long\\|const\\|static\\|friend\\)\\)*[ \t\n\r]+[a-zA-Z0-9_]+\\(<[a-zA-Z0-9_, \t\n\t]*>\\)?\\([ \t\n\r]+\\(const[ \t\n\r]+\\)\\)?\\([*&]\\|\\[[ \t\n\r]*[0-9]+[ \t\n\r]*\\]\\)*\\([ \t\n\r]+const\\)?\\)[ \t\n\r]*"
  "*The RE for a simple type in C++ that is types that neither involve
  blocks nor functions")

(defconst c++-function-regexp "[^a-zA-Z0-9_:]\\(\\([a-zA-Z0-9_]+\\(<[a-zA-Z0-9_, \t\n\r]*>\\)?::\\)*\\(\\(operator[ \t\n\r]*\\([-+*/.&^|=]\\|++\\|--\\|&&\\|||\\|new\\|delete\\|()\\|\\[\\]\\|->\\|[^();{}]+\\)=?\\)\\|~?[a-zA-Z0-e9_]+\\)\\)[ \t\n\r]*"
  "The RE for a function definition or declaration")

(defconst c++-class-regexp "\\(class\\|namespace\\)[ \t\n\r]+\\([a-zA-Z0-9_:]+\\)\\([ \t\n\r]*:\\([ \t\n\r]*\\(public\\|protected\\|private\\)?[ \t\n\r]*\\([a-zA-Z0-9_]+\\(<[^>]*>\\)?\\(::\\)?\\)*[ \t\n\r]*,?\\)*\\)?[ \t\n\r]*"
  "The RE for a class declaration")

(defconst c++-template-regexp "template[ \t\n\r]*<\\([^>]*\\)>[ \t\n\r]*"
  "The RE for matching template clauses.")

(defconst c++-function-keywords 
  '( "if" "while" "for" "repeat" "class" "struct" "union" "switch" )
  "C++ Keywords which may introduce a block")

(defconst c++-keywords 
  '( "if" "while" "for" "repeat" "class" "struct" "union" "typedef"
     "char" "int" "float" "double" "signed" "unsigned" "long" "const"
     "switch" "case" "repeat" "until" "do" "class" "public"
     "protected" "private" "friend" "operator" "void" "static" "explicit" )
  "C++ keywords")

(defvar c++-smart-wrap-column 80)

(require 'cc-cmds)
(require 'cc-engine)

(defun c++-scan-type-backward ()
  "Scans backward to find the longest valid simple type ending at POINT.
Assumes POINT is at the end of a C++ simple type expression. 
Leaves POINT at the beginning of the type expression and returns 
 ( TYPE-START . TYPE-END )"
  (save-excursion
    (let ((end (point))
	  start)
      (re-search-backward c++-simple-type-regexp 'nil 't)
      (catch 'exit
	(while (and (re-search-forward c++-simple-type-regexp end 't)
		    (< (abs (- end (match-end 0))) 2))
	  (goto-char (match-beginning 0))
	  (setq start (point))
	  (if (not (re-search-backward c++-simple-type-regexp 'nil 't)) 
	      (throw 'exit))))
      (if start
          (progn
	    (goto-char start)
            (skip-chars-forward " \t\n\r")
	    (setq start (point))
            (goto-char end)
            (skip-chars-backward " \t\n\r" start)
	    (cons start (point)))
	'nil ))))

(defun c++-get-function ()
  "Get the last function declared or called at or after POINT.
Returns a list 
        ( ( DECL-START . DECL-END ) 
          ( TYPE-START . TYPE-END ) 
	  ( NAME-START . NAME_END ) 
	  ( ARGLIST-START . ARGLIST-END )
          ( FLAGS-START . FLAGS-END ) )"
  (save-excursion
    (if (re-search-backward (concat c++-function-regexp "(") 'nil 'true)
	(goto-char (match-end 0)))
    (if (re-search-forward (concat c++-function-regexp "(") 'nil 't)
	(let (type name arglist flags)
	  (setq name (cons (match-beginning 1) (match-end 1)))
	  (goto-char (1- (match-end 0)))
	  (setq arglist (cons (point) 'nil))
	  (if (condition-case nil
		  (progn (forward-list 1) t)
		(scan-error nil))
	      (progn
		(setcdr arglist (point))
		(if (looking-at "\\([ \t\n\r]*\\(const\\|throw[ \t\n\r]*([^();{}]*)\\|__throw__[ \t\n\t]*(([^();{}]*))\\)\\)*")
		    (progn
		      (setq flags (cons (match-beginning 0) (match-end 0)))
		      (goto-char (match-end 0)))
		  (setq flags (cons (cdr arglist) (cdr arglist))))
		(if (looking-at "[ \t\n\r]*:[^;{]*")
		    (goto-char (match-end 0)))
		(if (looking-at "[ \t\n\r]*[{;]")
		    (progn
		      (goto-char (car name))
		      (setq type (c++-scan-type-backward))
		      (if (not type) (setq type (cons (car name) (car name))))
		      (list (cons (car type) (cdr flags)) type name arglist flags)))))))))

(defun c++-get-function-defn ()
  "Get the function defined at POINT.
Returns a list 
	( ( DECL-START . DECL-END ) 
	  ( TYPE-START . TYPE-END ) 
	  ( NAME-START . NAME_END ) 
	  ( ARGLIST-START . ARGLIST-END ) 
          ( FLAGS-START . FLAGS-END )
	  ( BODY-START . BODY-END) )"
  (save-excursion
    (let (body fn end)
      (while (not fn)
	(if (condition-case nil 
		(progn (up-list -1) t) 
	      (error (setq fn t) nil))
	    (save-excursion
	      (setq body (cons (point) (save-excursion 
					 (forward-list 1) 
					 (point))))
	      (save-excursion
		(while (condition-case nil
			   (progn 
			     (up-list -1) 
			     (setq end (point))
			     t)
			 (scan-error nil))))
	      (while (re-search-backward (concat c++-function-regexp "(") end t)
		(let ((tryfn (c++-get-function)))
		  (if tryfn
		      (save-excursion
			(goto-char (cdar tryfn))
			(if (and (looking-at "[ \t\n\r]*\\(:[^;{]*\\)?{")
				 (eq (1- (match-end 0)) (car body)))
			    (progn
			      (if (match-beginning 1)
				  (setcar body (match-beginning 1)))
			      (setq fn tryfn))))))))))
      (if (eq fn t)
	  nil
	(nconc fn (list body))))))

(defun c++-get-classname ()
  "Get classname which is active at POINT"
  (let (class
	(re 't))
    (save-excursion
      (while re
	(if (and (condition-case nil 
		     (progn (up-list -1) 't) 
		   (error (setq re 'nil)))
		 (re-search-backward  (concat c++-class-regexp "\\=") 'nil 't))
	    (setq class (concat (match-string 2)
				(if class "::" "")
				class )))))
    class))

(defun c++-get-current-class-prefix ()
  "Get the class prefix currently active at POINT.
If within a class decl, returns that class name (nested classes
are supported). If within a function definition at global level,
returns the class prefix of this function. Returns 'nil if no
class prefix can be found"
  (let ((x (c++-get-classname)))
    (if x
	x
      (let ((x (or (c++-get-function-defn)
		   (c++-get-function))))
	(if x
	    (save-excursion
	      (goto-char (car (nth 2 x)))
	      (if (re-search-forward "\\=\\([a-zA-Z_0-9]+::\\)+")
		  (buffer-substring (match-beginning 0) (- (match-end 0) 2))
		'nil))
	  'nil)))))
	    
(defun c++-split-template-arg (arg)
  "Split ARG, a template argument list, and return list of arg names."
  (let ((raw-args (split-string arg "[ \t\n\r]*,[ \t\n\r]")))
    (loop for raw-arg in raw-args
	  if (string-match "[a-zA-Z0-9_-]+$" raw-arg)
	    collect (match-string 0 raw-arg)
	  else
	    collect raw-arg)))

(defun c++-get-classname-with-templates ()
  (let (classname template-args
	(re 't))
    (save-excursion
      (while re
	(if (and (condition-case nil 
		     (progn (up-list -1) 't) 
		   (error (setq re 'nil)))
		 (re-search-backward  (concat c++-class-regexp "\\=") nil t))
	    (let ((template-suffix "")
		  (class (match-string 2)))
	      (if (re-search-backward (concat c++-template-regexp "\\=") nil t)
		  (let* ((s (match-string 1))
			 (args (c++-split-template-arg s))
			 (raw-args (split-string s "[ \t\n\r]*,[ \t\n\r]")))
		    (setq template-suffix
			  (concat "<" (mapconcat 'identity args ",") ">"))
		    (loop for arg in raw-args
			  if (not (member arg template-args))
			    do (setq template-args
				     (nconc template-args (list arg))))))
	      (setq classname (concat class
				      template-suffix
				      (if classname "::" "")
				      classname))))))
    (and classname
	 (cons classname
	       (and template-args
		    (mapconcat 'identity template-args ", "))))))

(defun c++-get-current-class-prefix-with-templates ()
  "Get the class prefix currently active at POINT.
If within a class decl, returns that class name (nested classes
are supported). If within a function definition at global level,
returns the class prefix of this function. Returns 'nil if no
class prefix can be found"
  (let ((x (c++-get-classname-with-templates)))
    (if x
	(car x)
      (let ((x (or (c++-get-function-defn)
		   (c++-get-function))))
	(if x
	    (save-excursion
	      (goto-char (car (nth 2 x)))
	      (if (re-search-forward "\\=\\([a-zA-Z_0-9]+\\(<[a-zA-Z0-9_, \t\n\r]*>\\)?::\\)+")
		  (buffer-substring (match-beginning 0) (- (match-end 0) 2))
		'nil))
	  'nil)))))

(defun c++-grab-inline-decl ()
  "Grab the inline declaration at POINT and change it into a standard
declaration.  This function deletes the declaration found at POINT and
places the new declaration into the top of the kill ring"
  (interactive)
  (let ((fn (c++-get-function-defn))
	(class (c++-get-classname-with-templates)))
    (if (and fn class)
	(progn
	  (kill-new (concat (if (cdr class)
				(concat "template <" (cdr class) ">\n")
			      "")
			    "inline "
			    (buffer-substring (car (nth 0 fn)) (car (nth 2 fn)))
			    (car class) "::"
			    (buffer-substring (car (nth 2 fn)) (cdr (nth 5 fn)))
			    "\n"))
	  (delete-region (cdr (nth 4 fn)) (cdr (nth 5 fn)))
	  (goto-char (cdr (nth 4 fn)))
	  (insert ";")
	  (delete-blank-lines))
      (if (interactive-p) 
	  (message "Not inside inline function definition body")))))

(defun c++-insert-class-prefix ()
  "Insert the current class prefix at POINT.
See also c++-get-current-class-prefix"
  (interactive)
  (let ((x (c++-get-current-class-prefix-with-templates)))
    (if x
	(insert x "::")
      (message "Not in scope of any class"))))

(defun c++-next-user-symbol ()
  "Move POINT to next non-keyword symbol."
  (interactive)
  (let (pos)
    (if (looking-at "[a-zA-Z_:]+")
	(setq pos (re-search-forward "[a-zA-Z_:]+" 'nil 't)))
    (while (and (setq pos (re-search-forward "[a-zA-Z_:]*[a-zA-Z_]" 'nil 't))
		(or (member (match-string 0) c++-keywords)
		    (c-in-literal))))
    (goto-char pos)))

(defun c++-previous-user-symbol ()
  "Move POINT to previous non-keyword symbol."
  (interactive)  
  (while (and (re-search-backward "[a-zA-Z_]" 'nil 't)
	      (progn (skip-chars-backward "a-zA-Z_:") 
		     (or (member (buffer-substring (point) (match-end 0)) 
				 c++-keywords)
			 (c-in-literal))))))

(defun c++-next-function-call ()
  "Move POINT to next function call"
  (interactive)
  (let ((start (point))
	fn)
    (while (and (setq fn (c++-get-function))
		(or (>= start (car (nth 0 fn)))
		    (member (buffer-substring (car (nth 2 fn)) (cdr (nth 2 fn)))
			    c++-function-keywords)
		    (/= (car (nth 1 fn)) (car (nth 2 fn)))))		    
      (goto-char (cdr (nth 0 fn)))
      (re-search-forward "{;" 'nil 't))
    (goto-char (if fn (car (nth 0 fn)) start))))
    
(defun c++-next-function-definition ()
  "Move POINT to the next function definition or declaration"
  (interactive)
  (let ((start (point))
	fn)
    (while (and (setq fn (c++-get-function))
		(or (>= start (car (nth 0 fn)))
		    (member (buffer-substring (car (nth 2 fn)) (cdr (nth 2 fn)))
			    c++-function-keywords)
		    (= (car (nth 1 fn)) (car (nth 2 fn)))))		    
      (goto-char (cdr (nth 0 fn)))
      (re-search-forward "[{;]" 'nil 't))
    (goto-char (if fn (car (nth 0 fn)) start))))
  
(defun c++-smart-yank ()
  "Yank-pop top of kill ring and reformat the yanked object."
  (interactive)
  (push-mark)
  (let ((text (current-kill 0))
	end-mark line-start)
    (save-excursion
      (insert text)
      (setq end-mark (point-marker)))
    (while (< (point) (marker-position end-mark))
      (beginning-of-line)
      (c-indent-command)
      (setq line-start (point))
      (end-of-line)
      (if (> (current-column) c++-smart-wrap-column)
	  (progn
	    (move-to-column c++-smart-wrap-column)
	    (if (search-backward ",")
		(progn
		  (forward-char 1)
		  (open-line 1)))))
      (forward-line 1))))

(provide 'c++)
