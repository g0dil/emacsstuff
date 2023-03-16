;; Miscellaneous local functions
;;
;; $Id: misc-local.el,v 1.20 2003/08/04 13:27:17 admin Exp $

;;; Change-Log:

;; $Log: misc-local.el,v $
;; Revision 1.20  2003/08/04 13:27:17  admin
;; Import der neuen Version
;;
;; Revision 1.19  2000/02/08 20:27:36  bund
;; Besseres *scratch*-Buffer handling in kill-most-buffers
;; setf in put-hashq/put-hash
;;
;; Revision 1.18  2000/02/04 13:16:45  bund
;; *scratch*-Buffer verwaltung in kill-most-buffers verbessert
;;
;; Revision 1.17  2000/02/03 09:10:53  bund
;; put-hash
;;
;; Revision 1.16  2000/02/01 11:21:18  bund
;; Added emi-split-string
;; Added put-hashq
;;
;; Revision 1.15  2000/01/26 15:26:58  bund
;; better prefix-arg handling in kill-most-buffers
;;
;; Revision 1.14  2000/01/12 14:38:42  bund
;; Neue funktion: kill-most-buffers
;;
;; Revision 1.13  1999/06/03 15:46:17  bund
;; Added template-document-name function
;;
;; Revision 1.12  1999/02/02 12:48:45  bund
;; Neue implementiereung von date-time-fielname-string et. al.
;;
;; Revision 1.11  1998/10/10 09:35:20  admin
;; BUGFIX: fixed hook dependencies
;;
;; Revision 1.10  1998/10/07 14:33:28  bund
;; Implemented window-size-change-functions hook for dedicated windows
;;
;; Revision 1.9  1998/10/07 13:44:46  bund
;; Implemeted open-dedicated-window and it's friends
;;
;; Revision 1.8  1998/06/17 15:58:30  bund
;; added query-filename-and-check
;;
;; Revision 1.7  1998/06/05 16:22:45  bund
;; added load-add-hook-or-run
;;
;; Revision 1.7  1998/05/17 15:51:38  bund
;; moved adabas-convert-char-byte to dataface.el
;;
;; Revision 1.6  1998/04/23 17:09:52  bund
;; implemented hsxstring conversion functions
;;
;; Revision 1.5  1998/04/16 11:52:48  bund
;; fixed (args-out-of-range) bug in string-replace
;;
;; Revision 1.4  1998/04/14 09:42:45  bund
;; Implemented grep-list and grep-map-list[*]
;;
;; Revision 1.3  1998/03/31 12:47:28  bund
;; misc changes
;;
;; Revision 1.2  1998/03/30 08:39:34  bund
;; Added emi-mapcar*
;;

;;; Variables:

(defvar kill-most-buffers-nokill-list
  '("*desktop*" " *Adabas*"))

;;; Code:

(require 'cl)

(defun emi-mapcar* (f &rest args)
  "Apply FUNCTION to successive cars of all ARGS.
Return the list of results."
  (if (not (memq 'nil args))
      (cons (apply f (mapcar 'car args))
            (apply 'emi-mapcar* f
                   (mapcar 'cdr args)))))

(defun string-noempty (str)
  "Return STR if (length STR) > 0, nil otherwise"
  (if (> (length str) 0)
      str
    nil))

(defun date-time-filename-string (date &optional num)
  "Return the string YYYYMMDDHHMMSS[nn] for the date DATE.
 Append nn if NUM is non-nil."
  (concat (format "%04d%02d%02d%02d%02d%02d"
                  (nth 5 date) (nth 4 date) (nth 3 date)
                  (nth 2 date) (nth 1 date) (nth 0 date))
          (if num (format "%02d" num) "")))

(defun date-time-string (date)
  "Return DD.MM.YYYY HH:MM for the date DATE."
  (format "%2d.%02d.%04d %2d:%02d"
          (nth 3 date) (nth 4 date) (nth 5 date)
          (nth 2 date) (nth 1 date)))

(defun current-date-time-filename-string (&optional num)
  "Return the string YYYYMMDDHHMMSS[nn] for the current date-and-time.
If called interactively insert string at POINT"
  (interactive)
  (if (interactive-p)
      (insert (date-time-filename-string (decode-time (current-time))))
    (date-time-filename-string (decode-time (current-time)))))

(defun current-date-time-string ()
  "Return the string DD.MM.YYYY HH:MM for the current date-and-time."
  (interactive)
  (if (interactive-p)
      (insert (date-time-string (decode-time (current-time))))
    (date-time-string (decode-time (current-time)))))

(defun template-document-name (template)
  (if (string-match "\\.tmpl$" template)
      (substring template 0 (match-beginning 0))
    template))

(defun template-document-extension (template)
  "Return the extension of TEMPLATE without the trailing .tmpl.
That is, if TEMPLATE is 'filename.ext.tmpl', return '.ext'. If
TEMPLATE does not have the '.tmpl' extension or the '.ext' part is
mising, return nil"
  (if (string-match "\\.tmpl$" template)
      (progn
        (setq template (replace-match "" t t template))
        (if (string-match "^.*\\." template)
            (replace-match "" t t template)
          nil))
    nil))

(defun read-file-name-with-default (prompt default &optional existing)
  "Read file name from user prompting with PROMPT. The user
will get DEFAULT provided as default choice"
  (let (dir name)
    (save-match-data
      (if (string-match "^.*/" default)
          (progn
            (setq dir (match-string 0 default)
                  name (substring default (match-end 0))))
        (setq dir ""
              name default)))
    (read-file-name prompt
                    dir
                    default
                    existing
                    name)))

(defun misc-re-string-replace (from to string &optional n start fixedcase literal subexp)
  "Replate first N occurences, all if T, one if NIL of FROM in STRING
with TO. Returns the new string. FIXEDCASE, LITERAL and SUBEXP have
the same meaning as in replace-match."
  (if (not (or (numberp n) n))
      (setq n 1))
  (while (and (if (numberp n) (> n 0) t)
              (or (not start) (< start (length string)))
              (string-match from string start))
    (setq start (- (match-end 0) (length string))
          string (replace-match to fixedcase literal string subexp)
          start (+ start (length string)))
    (if (numberp n)
        (setq n (1- n))))
  string)

(defun emi-split-string (string separator &optional N)
  "Split STRING at SEPARATOR (a regex). Return list of strings. If N
is given, split at most that many times. The last string return will
contain the remaining string."
  (let ((start 0)
        strings)
    (while (and (or (not N) (> N 0))
                (string-match separator string start))
      (setq strings (cons (substring string start (match-beginning 0))
                          strings)
            start (match-end 0)
            N (if N (- N 1))))
    (nreverse (cons (substring string start) strings))))

(defun grep-list (func list)
  "Create a new list from LIST keeping only elements, for which
FUNC returns non-nil."
  (if list
      (if (funcall func (car list))
          (cons (car list)
                (grep-list func (cdr list)))
        (grep-list func (cdr list)))))

(defun grep-map-list (func list)
  "Apply FUNC to all elements of LIST and build a new list from the
return values of FUNC (like mapcar) excluding all nil elements."
  (if list
      (let ((elem (funcall func (car list))))
        (if elem
            (cons elem
                  (grep-map-list func (cdr list)))
          (grep-map-list func (cdr list))))))

(defun grep-map-list* (func &rest args)
  "grep-map-list* is to grep-map-list, what emi-mapcar* is to mapcar."
  (if (not (memq nil args))
      (let ((elem (apply func (mapcar 'car args))))
        (if elem
            (cons elem
                  (apply 'grep-map-list* func (mapcar 'cdr args)))
          (apply 'grep-map-list* func (mapcar 'cdr args))))))

(defun hex-to-nibble (d)
  (if (and (not (string< d "0"))
           (or (string< d "9")
               (string= d "9")))
      (- (string-to-char d) ?0)
    (+ (- (string-to-char (upcase d)) ?A) 10)))

(defun hex-to-byte (string)
  (+ (if (> (length string) 0) (* (hex-to-nibble (substring string 0 1)) 16) 0)
     (if (> (length string) 1) (hex-to-nibble (substring string 1 2)) 0)))

(defun nibble-to-hex (n)
  (if (< n 10)
      (char-to-string (+ ?0 n))
    (char-to-string (+ ?A (- n 10)))))

(defun byte-to-hex (n)
  (concat (nibble-to-hex (% (/ n 16) 16))
          (nibble-to-hex (% n 16))))

(defun string-to-hexstring (value)
  (let ((v ""))
    (while (> (length value) 0)
      (setq v (concat v (byte-to-hex (string-to-char value)))
            value (substring value 1)))
    v))

(defun hexstring-to-string (value)
  (let ((v ""))
    (while (> (length value) 1)
      (setq v (concat v (char-to-string (hex-to-byte value)))
            value (substring value 2)))
    v))

(defun load-hook-add-or-run (feature hook-symbol hook)
  "If FEATURE is present, immediately execute HOOK, otherwise add it to
HOOK-SYMBOL (preferably a load hook symbol)"
  (if (featurep feature)
      (funcall hook)
    (add-hook hook-symbol hook)))

(defun query-filename-and-check (prompt &optional directory default initial)
  "Query the user for the name of a new file. If FILENAME allready exists,
query wether to overwrite it and delete the file in the affirmative case.
Returns the filename entered. If the user terminates the request, a quit
condition is generated."
  (let* ((filename (read-file-name prompt directory default nil initial))
         (filebuffer (find-buffer-visiting filename))
         (fileexists (file-readable-p filename)))
    (if (not (if (or filebuffer fileexists)
                 (yes-or-no-p (concat "Overwrite " filename "? "))
               t))
        (setq quit-flag t)
      (if fileexists (delete-file filename))
      (if filebuffer (kill-buffer filebuffer)))
    filename))

(defvar assign-window-buffer-window nil)
(defvar assign-window-buffer-buffers nil)
(put 'assign-window-buffer-window 'permanent-local t)
(put 'assign-window-buffer-buffers 'permanent-local t)
(make-variable-buffer-local 'assign-window-buffer-window)
(make-variable-buffer-local 'assign-window-buffer-buffers)
(put 'kill-buffer-hook 'permanent-local t)
(defvar assign-window-windows nil)
(defvar assign-window-hook-running nil)

(defun assign-window-to-buffer (buffer window &optional window-conf other-buffers)
  "Assigns WINDOW to be fixed on displaying BUFFER. If BUFFER is
killed, the WINDOW is killed to. If WINDOW-CONF is given, instead of
killing the buffer, the WINDOW-CONFiguration is restored. If
OTHER-BUFFERS is given, theese buffers are killed together with
BUFFER, if BUFFER is killed."
  (select-window window)
  (switch-to-buffer buffer)
  (setq buffer (get-buffer buffer))
  (make-local-hook 'kill-buffer-hook)
  (setq assign-window-buffer-window (cons window window-conf)
        assign-window-buffer-buffers
        (delq buffer (mapcar (function
                              (lambda (buffer)
                                (get-buffer buffer)))
                             other-buffers)))
  (add-hook 'kill-buffer-hook 'assign-window-to-buffer-hook t t)
  (setq assign-window-windows (cons (cons window buffer) assign-window-windows))
  (set-window-dedicated-p window t))

;;;FIXME: There's an emacs bug: If the dedicated window is the
;;;       right/top one of a split, killing the dedicated window will
;;;       result in the combined window having the dedicated flag
;;;       set. Workaround ???
(defun assign-window-change-hook (frame)
  (let ((p assign-window-windows)
        (assign-window-hook-running t)
        last)
    (while p
      (if (not (window-live-p (car (car p))))
          (progn
            (if (buffer-live-p (cdr (car p)))
                (kill-buffer (cdr (car p))))
            (if last
                (setcdr last (cdr p))
              (setq assign-window-windows (cdr p)))
            (setq p (cdr p)))
        (setq last p
              p (cdr p))))))

(if (not (memq 'assign-window-change-hook window-size-change-functions))
    (setq window-size-change-functions
          (cons 'assign-window-change-hook window-size-change-functions)))

(defun assign-window-to-buffer-hook ()
  (if (and (boundp 'assign-window-buffer-window)
           (boundp 'assign-window-buffer-buffers))
      (let ((window assign-window-buffer-window)
            (buffers assign-window-buffer-buffers)
            (old-assign-window-hook-running (and (boundp 'assign-window-hook-running)
                                                 assign-window-hook-running))
            (assign-window-hook-running t))
        (setq assign-window-windows
              (delete-if (function
                          (lambda (x) (eq (cdr x) (current-buffer))))
                         assign-window-windows))
        (if (window-live-p (car window))
            (set-window-dedicated-p (car window) nil))
        (if (cdr window)
            (if (not old-assign-window-hook-running)
                (set-window-configuration (cdr window)))
          (if (window-live-p (car window))
              (delete-window (car window))))
        (if buffers
            (mapcar (function (lambda (buffer)
                                (if (buffer-live-p buffer) (kill-buffer buffer))))
                    buffers)))))

(defun save-split-window (&optional size horizontal)
  "Split the current window vertically, horizontally if HORIZONTAL is
non-nil, if the size of the current frame permits.

size is passed to split-window-[horizontally|vertically] but adjusted
using window-min-width or window-min-height respectively.

The selected window will be the old one, i.e. the left/top one. The
return value will be the new window, or nil if the window was not
split."
  (if (if horizontal
          (> (window-width) (+ (* 2 window-min-width) 2))
        (> (window-height) (+ (* 2 window-min-height) 1)))
      (progn
        (if size
            (if (< size 0)
                (if horizontal
                    (setq size (- (max window-min-width (- size))))
                  (setq size (- (max window-min-height (- size)))))
              (if horizontal
                  (setq size (max window-min-width size))
                (setq size (max window-min-height size)))))
        (if horizontal
            (split-window-horizontally size)
          (split-window-vertically size)))))

(defun open-dedicated-window (buffer &optional size horizontal)
  "Open a new window visiting BUFFER. This new window will be assign
to BUFFER using assign-window-to-buffer. If SIZE is given, it gives
the size of the new window to open. By default the current window is
split vertically. If HORIZONTAL is non-nil, the window is split
horizontally.

If SIZE is positive, the left/top window after splitting will be the
new window, if SIZE is negative, the right/bottom window will be
used. if SIZE is not nil and not a number, the right/bottom window
will be used, but no explicit SIZE is requested.

The selected buffer and window will be the newly opened window with
it's bufer. The return value will be the window showing the buffer
active before calling this function. If the window could not be split,
because the frame is to small, BUFFER will be the selected buffer in
the current window and the return value is nil."
  (let ((size (and (numberp size) size))
        (which (if (numberp size) (< size 0) size))
        (wc (current-window-configuration))
        this other)
    (if (setq other (save-split-window size horizontal))
        (if which
            (progn
              (setq this other
                    other (selected-window))
              (select-window this))
          (setq this (selected-window))))
    (assign-window-to-buffer buffer (selected-window) wc)
    other))

(defun kill-most-buffers (arg)
  "Kill all Buffers exept those in kill-most-buffers-nokill-list.

If called with a negative prefix- argument, the current buffer will
not be killed. If called with a positive prefix argument only
non-displayed buffers are killed.

Additionally will make all windows in all frames schow the `*scratch*'
buffer."
  (interactive "P")
  (loop for buffer being the buffers
        if (not (or (and arg
                         (if (> (prefix-numeric-value arg) 0)
                             (get-buffer-window buffer t)
                           (eq buffer (current-buffer))))
                    (member (buffer-name buffer)
                            kill-most-buffers-nokill-list)))
          do (kill-buffer buffer))
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (get-buffer-create "*scratch*")
  (if (not (and arg (> (prefix-numeric-value arg) 0)))
      (loop for window being the windows
            if (not (and arg (eq window (selected-window))))
              do (set-window-buffer window "*scratch*"))))

(defmacro put-hashq (element hash)
  "Place ELEMENT into HASH."
  (let ((x (make-symbol "x"))
        (y (make-symbol "y")))
    `(let* ((,x ,element)
            (,y (assq (car ,x) ,hash)))
       (if ,y
           (progn (setcdr ,y (cdr ,x)) ,y)
         (progn (setf ,hash (cons ,x ,hash)) ,x)))))

(defmacro put-hash (element hash)
  "Place ELEMENT into HASH."
  (let ((x (make-symbol "x"))
        (y (make-symbol "y")))
    `(let* ((,x ,element)
            (,y (assoc (car ,x) ,hash)))
       (if ,y
           (progn (setcdr ,y (cdr ,x)) ,y)
         (progn (setf ,hash (cons ,x ,hash)) ,x)))))

(provide 'misc-local)
