;;; cc-ide.el --- Add all g0dilstuff lisp dirs to the load path
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

(require 'cl)

(let* ((self (locate-library "g0dilstuff-init"))
       (self-dir (file-name-directory self)))
  (loop for dir in (directory-files self-dir t)
	if (file-directory-p dir) do (add-to-list 'load-path (file-name-as-directory dir))))

(provide 'g0dilstuff-init)
