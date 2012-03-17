;;; lice.el --- License Template

;; Copyright (C) 2012  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'newcomment)

(defconst lice:default-template-directory
  (expand-file-name "template"
		    (or (and load-file-name (file-name-directory load-file-name))
			default-directory)))

(defgroup lice nil
  "License Template"
  :prefix "lice:")

(defcustom lice:license-directories
  (list lice:default-template-directory)
  "A location of License template directories"
  :group 'lice
  :type '(repeat directory))

(defcustom lice:comment-style 'extra-line
  "A comment style for license insertion.
Use `comment-style' value when this is nil."
  :group 'lice
  :type `(choice
	  ,@(loop for x in comment-styles
		  collect `(const
			    :tag ,(replace-regexp-in-string
				   "-" " " (capitalize (symbol-name (car x))))
			    ,(car x)))
	  (other :tag "Mode Default" nil)))

(defcustom lice:default-license "gpl-3.0"
  "A default license name"
  :group 'lice
  :type 'string)

(defcustom lice:coypright-function 'lice:default-copyright
  "A copyright inserter function."
  :group 'lice
  :type 'function)

(defcustom lice:mode-comments
  (loop for mode in '(c-mode c++-mode java-mode groovy-mode)
	collect (list mode :comment-start "/*" :comment-end "*/"))
  "A definition of mode specific comments.
Each elements are follows:
\(MODE :comment-start COMMENT-START :comment-end COMMENT-END))"
  :group 'lice
  :type '(repeat (list :format "%v\n"
		  (function :tag "Mode" :size 20)
		       (const :format "" :comment-start)
		       (string :tag " Comment Start" :size 5)
		       (const :format "" :comment-end)
		       (string :tag " End" :size 5))))

(defvar lice:license-history nil)

(defun lice:licenses ()
  "Return a license list.
Each element are follows:
\(SIMPLE-NAME . FILE)"
  (loop for dir in lice:license-directories
	with licenses
	if (and dir (file-directory-p dir))
	append (lice-directory-licenses dir) into licenses
	finally return (sort licenses
			     (lambda (a b) (string< (car a) (car b))))))

(defun lice:directory-licenses (dir)
  (loop for file in (directory-files dir t)
	with licenses
	for name = (file-name-nondirectory file)
	if (and (file-regular-p file) (not (assoc name licenses)))
	collect (cons name file)))

(defun lice:default-copyright (name)
  (format "Copyright (C) %s  %s\n\n"
	  (format-time-string "%Y") (user-full-name)))

(defun lice:insert-license (name)
  (interactive (list (lice:read-license)))
  (let ((license (assoc name (lice:licenses))))
    (unless license
      (error "Unknown license name: %s" name))
    (goto-char (point-min))
    (insert (with-temp-buffer
	      (insert (funcall lice:coypright-function name))
	      (insert-file-contents (cdr license))
	      (buffer-string)))
    (lice:comment-region (point-min) (point) major-mode)))

(defun lice:read-license ()
  (completing-read (format "License Name (%s): " lice:default-license)
		   (lice:licenses)
		   nil t nil 'lice:license-history
		   lice:default-license))

(defun lice:comment-region (start end mode)
  (let* ((comment (cdr (assq mode lice:mode-comments)))
	 (comment-start (or (plist-get comment :comment-start) comment-start))
	 (comment-end (or (plist-get comment :comment-end) comment-end))
	 (comment-style (or lice:comment-style comment-style)))
    (comment-region start end)))

(provide 'lice)
;;; lice.el ends here
