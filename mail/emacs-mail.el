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

;;; Code:

(defun mail-add-header (header value &optional separator)
  (when (and value (> (length value) 0))
    (mail-position-on-field header)
    (if (save-excursion
          (skip-chars-backward " \t\n\r")
          (not (eq (preceding-char) ?:)))
        (insert separator))
    (insert value)))

(defun write-mail (&optional to subject cc bcc body attachment)
  (interactive)
  (loop for b in (buffer-list)
        if (eq major-mode 'message-mode)
            return (pop-to-buffer b)
        finally (progn
                  (if (eq major-mode 'gnus-summary-mode)
                      (gnus-summary-mail-other-window)
                    (gnus-group-mail))
                  (goto-char (point-max))
                  (if (re-search-backward "^-- $" nil)
                      (forward-line -1))))
  (save-excursion
    (mail-add-header "To" to ",\n  ")
    (mail-add-header "Subject" subject "; ")
    (mail-add-header "CC" cc ", ")
    (mail-add-header "BCC" bcc ", "))
  (if body (insert body))
;;       (when (file-readable-p "~/.signature")
;;         (insert "\n-- \n")
;;         (insert-file "~/.signature")
;;         (goto-char (point-max))
;;         (if (not (bolp))
;;             (insert "\n")))
  (when (and attachment (> (length attachment) 0))
    (if (and (> (length attachment) 5)
             (string= (substring attachment 0 5) "file:"))
        (setq attachment (substring attachment 5)))
    (when (file-readable-p attachment)
      (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (mime-edit-insert-file attachment))))
  (font-lock-fontify-buffer))

(provide 'emacs-mail)
