;;; softtext.el --- Load/Save translator for soft text files
;;
;; $Id$
;;
;; Copyright (C) 1999 Stefan Bund

;; softtext.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; softtext.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Externally soft newlines are represented as ordinary newlines with
;; a preceding space. For this to work reliably, spaces before
;; newlines are removed prior to writing a buffer out to it's external
;; representation and re-inserted as annotations in the buffer
;; encoding function.

;;; Change-Log:

;; $Log$
;;

;;; Variables:

;;; Code:

(defun softtext-convert-external-to-internal (start end)
  (let ((mod (buffer-modified-p)))
    (save-excursion
      (goto-char start)
      (if (save-excursion (search-forward "\r\n" nil t))
          ;;; Old version compatibility
          (while (search-forward "\r\n" end t)
            (replace-match "\n")
            (set-hard-newline-properties (1- (point)) (point)))
        (while (search-forward "\n" end t)
	  (if (and (> (match-beginning 0) start)
		   (eq (string-to-char (buffer-substring-no-properties (1- (match-beginning 0))
									   (match-beginning 0)))
		       ? ))
	      (save-excursion
		(goto-char (1- (match-beginning 0)))
		(delete-char 1))
	    (set-hard-newline-properties (match-beginning 0)
					 (match-end 0))))))
    (set-buffer-modified-p mod)))

(defun softtext-convert-internal-to-external (start end &optional buffer)
  (let ((mod (buffer-modified-p))
	annotations)
    (save-excursion
      (if buffer (set-buffer buffer))
      (save-excursion
        (goto-char start)
        (while (search-forward "\n" end t)
          (if (not (get-text-property (1- (point)) 'hard))
	      (if buffer
		  (setq annotations (cons (cons (- (point) start) " ")
					  annotations))
		(forward-char -1)
		(insert " ")
		(forward-char 1))))))
    (if (not buffer) (set-buffer-modified-p mod))
    (nreverse annotations)))

(defun softtext-cleanup-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " *\n" nil t)
      (let ((blanks (- (match-end 0) (match-beginning 0) 1)))
	(if (> blanks 0)
	    (progn
	      (goto-char (match-beginning 0))
	      (delete-char blanks)
	      (forward-char 1)))))))

(defun softtext-hard-newlines-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" end t)
      (set-hard-newline-properties (1- (point)) (point)))))

(let ((x (assq 'softtext format-alist)))
  (if x (setq format-alist (delq x format-alist))))

(let ((elt (assq 'softtext format-alist)))
  (if elt (setq format-alist (delq elt format-alist)))
  (setq format-alist
	(cons '(softtext "Text with hard/soft newlines" nil
			 softtext-convert-external-to-internal
			 softtext-convert-internal-to-external
			 nil nil)
	      format-alist)))

(define-minor-mode softwrap-mode
  "Toggle SoftWrap minor mode.
With no argument, this command toggles SoftWrap mode. A Non-null
prefix argument enables the mode, a null prefix argument disables it.

In SoftWrap mode, emacs differentiates between hard and soft
newlines. On writing out the buffer to its file, soft newlines a
reconverted to ordinary newlines preceded by a single space. Any other
whitspace preceding a newline is removed."
  nil
  " SoftWrap"
  nil
  (if softwrap-mode
      (when (not (memq 'softtext buffer-file-format))
	(softtext-convert-external-to-internal (point-min) (point-max))
	(setq buffer-file-format (append buffer-file-format (list 'softtext)))
	(setq use-hard-newlines t)
	(add-hook 'write-contents-hooks 'softtext-cleanup-buffer))
    (when (memq 'softtext buffer-file-format)
      (remove-hook 'write-contents-hooks 'softtext-cleanup-buffer)
      (setq use-hard-newlines nil)
      (setq buffer-file-format (delq 'softtext buffer-file-format))
      (softtext-convert-internal-to-external (point-min) (point-max)))))

(define-derived-mode softtext-mode indented-text-mode "Text"
  "SoftText Mode is indented-text-mode with SoftWrap minor mode enabled"
  (softwrap-mode 1))

(provide 'softtext)
