;;; lice.el --- License And Header Template -*- lexical-binding: t -*-

;; Copyright (C) 2012  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: template, license, tools
;; URL: https://github.com/buzztaiki/lice-el

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

;; Overview
;; --------

;; `lice.el` provides following features:

;; - License template management.
;; - File header insertion.

;; Usage
;; -----

;; Usage is very easy, put `lice.el` in your Emacs system, and open a new
;; file, and run:

;;     M-x lice

;; Then, `lice.el` tell to use which license (default is gpl-3.0).  You
;; can select license on minibuffer completion.

;; When you select license, and enter the `RET`, license and copyright is
;; putted into a text.

;; More Information
;; ----------------

;; See the `README.md` file for more information.

;;; Code:

(require 'cl-lib)
(require 'newcomment)

(defconst lice:version "0.3")

(defconst lice:system-template-directory
  (expand-file-name "template"
                    (or (and load-file-name (file-name-directory load-file-name))
                        default-directory)))

(defvar lice:custom-template-directory
  (expand-file-name "lice" user-emacs-directory))


(defgroup lice nil
  "License And Header Template"
  :prefix "lice:"
  :group 'tools
  :link '(url-link "https://github.com/buzztaiki/lice-el"))

(define-widget 'lice:comment-style 'choice
  "The comment style selection widget."
  :args `(,@(cl-loop for x in comment-styles
                     collect `(const
                               :tag ,(replace-regexp-in-string
                                      "-" " " (capitalize (symbol-name (car x))))
                               ,(car x)))
          (other :tag "Mode Default" nil)))

(defcustom lice:license-directories
  (list lice:system-template-directory lice:custom-template-directory)
  "The location of License template directories."
  :group 'lice
  :type '(repeat directory))

(defcustom lice:comment-style 'extra-line
  "The comment style for license insertion.
When nil, `comment-style' value is used."
  :group 'lice  :type 'lice:comment-style)

(defcustom lice:default-license "gpl-3.0"
  "The default license name."
  :group 'lice
  :safe 'stringp
  :type 'string)

(defcustom lice:program-name nil
  "The name of the program."
  :group 'lice
  :safe 'stringp
  :type 'string)

(defcustom lice:program-description nil
  "One line to give the program's name and a brief idea of what it does."
  :group 'lice
  :safe 'stringp
  :type 'string)

(defcustom lice:copyright-holder (user-full-name)
  "The copyright holder."
  :group 'lice
  :safe 'stringp
  :type 'string)

(defcustom lice:header-spec '(lice:insert-description
                              lice:insert-copyright
                              lice:insert-license)
  "The license header spec.
Each element should be function.
These functions should take one argument, license object, and
should insert header string fragment."
  :group 'lice
  :type '(repeat function))

(defcustom lice:mode-comments
  (append
   (cl-loop for mode in '(c-mode c++-mode java-mode groovy-mode)
            collect (list mode
                          :comment-start "/*"
                          :comment-end "*/"))
   '((nxml-mode :comment-continue "   ")))
  "The definition of mode specific comments.
Each elements are follows:
  \(MODE . PROPERTIES))
Mode is a `major-mode' which is applied PROPERTIES.
PROPERTIES is a plist whitch has following properties:
  :comment-start - `comment-start' of this MODE.
  :comment-end   - `comment-end' of this MODE.
  :comment-style - `comment-style' of this MODE.
  :comment-continue - `comment-continue' of this MODE."
  :group 'lice
  :type '(repeat (cons :format "%v" :indent 9
                       (function :tag "Mode" :size 20)
                       (sexp :tag ""
                             :value-to-internal (lambda (widget value)
                                                  (prin1-to-string value))))))

(defvar lice:license-history nil)

(defun lice:licenses ()
  "Return a license list.
Each element are follows:
\(SIMPLE-NAME . FILE)"
  (cl-loop for dir in lice:license-directories
           with licenses
           if (and dir (file-directory-p dir))
           do (let* ((dir-licenses (lice:directory-licenses dir))
                     (dir-licenses (cl-delete-if (lambda (x) (assoc (car x) licenses)) dir-licenses)))
                (setq licenses (append dir-licenses licenses)))
           finally return (sort licenses
                                (lambda (a b) (string< (car a) (car b))))))

(defun lice:directory-licenses (dir)
  (cl-loop for file in (directory-files dir t)
           for name = (file-name-nondirectory file)
           if (file-regular-p file)
           collect (cons name file)))

;;;###autoload
(defun lice (name &optional year)
  "Insert license and headers.
NAME is a template name for insertion."
  (interactive (list
                (lice:read-license)
                (if current-prefix-arg (lice:read-start-year))))
  (let ((license (assoc name (lice:licenses))))
    (unless license
      (error "Unknown license name: %s" name))
    (save-restriction
      (narrow-to-region (point) (point))
      (cl-loop for component in lice:header-spec
               do (progn (funcall component license year)
                         (goto-char (point-max))))
      (when (lice:comment-enabled-p major-mode)
        (lice:comment-region (point-min) (point-max) major-mode))
      (goto-char (point-max)))))

(defun lice:insert-description (_license _year)
  (when lice:program-description
    (insert (format "%s\n" lice:program-description))))

(defun lice:insert-copyright (_license year)
  (let* ((current-year (format-time-string "%Y"))
         (years-string (if year
                           (format "%s-%s" year current-year)
                         current-year)))
    (insert (format "Copyright (C) %s  %s\n\n"
                    years-string
                    lice:copyright-holder))))

(defun lice:insert-license (license _year)
  (insert-file-contents (cdr license))
  (lice:update-license-for-gpl license)
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (delete-region (point) (point-max))
  (insert "\n"))

(defun lice:update-license-for-gpl (license)
  ;; see https://www.gnu.org/licenses/gpl-howto.html
  (when (and lice:program-name
             (member (car license) '("gpl-2.0" "gpl-3.0" "agpl-3.0" "lgpl-3.0")))
    (insert (format "This file is part of %s.\n\n" lice:program-name))
    (while (re-search-forward "[Tt]his program" nil t)
      (replace-match lice:program-name))))

(defun lice:read-license ()
  (completing-read (format "License Name (%s): " lice:default-license)
                   (lice:licenses)
                   nil t nil 'lice:license-history
                   lice:default-license))

(defun lice:read-start-year ()
  (let* ((current-year (caddr (calendar-current-date)))
         (prev-year (1- current-year))
         (year nil))
    (while
        (progn
          (setq year (or (read-number (format "Start Year: " prev-year) prev-year)))
          (unless (< year current-year)
            (message "Please enter a year lower than the current year.")
            (sit-for 1)
            t)))
    (number-to-string year)))

(defun lice:mode-comment (mode)
  (and mode
       (or (cdr (assq mode lice:mode-comments))
           (lice:mode-comment (get mode 'derived-mode-parent)))))

(defun lice:comment-region (start end mode)
  (let* ((comment (lice:mode-comment mode))
         (comment-start (or (plist-get comment :comment-start) comment-start))
         (comment-end (or (plist-get comment :comment-end) comment-end))
         (comment-style (or (plist-get comment :comment-style)
                            lice:comment-style
                            comment-style))
         (comment-continue (or (plist-get comment :comment-continue) comment-continue)))
    (save-restriction
      (narrow-to-region start end)
      (comment-region (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max)))))

(defun lice:comment-enabled-p (mode)
  (let ((comment (lice:mode-comment mode)))
    ;; A `comment-start' indicates that the `mode' supports it or not (see `comment-normalize-vars').
    (or (plist-get comment :comment-start) comment-start)))

(provide 'lice)
;;; lice.el ends here
