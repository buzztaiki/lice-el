;;; lice-tests.el --- Tests for lice.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ert)
(require 'lice)

(ert-deftest test-lice-licenses ()
  (let ((lice:license-directories (list lice:system-template-directory)))
    (should (> (length (lice:licenses)) 1))
    (should (equal (assoc "gpl-3.0" (lice:licenses))
                   (cons "gpl-3.0" (expand-file-name "gpl-3.0" lice:system-template-directory))))))

(ert-deftest test-lice-licenses-uniqueness ()
  (let* ((dir (expand-file-name (make-temp-name "test-lice") temporary-file-directory))
         (lice:license-directories (list lice:system-template-directory dir)))
    (unwind-protect
        (progn
          (make-empty-file (expand-file-name "gpl-3.0" dir) t)
          (make-empty-file (expand-file-name "supercow-99" dir) t)
          (should (equal (assoc "gpl-3.0" (lice:licenses))
                         (cons "gpl-3.0" (expand-file-name "gpl-3.0" lice:system-template-directory))))
          (should (equal (assoc "supercow-99" (lice:licenses))
                         (cons "supercow-99" (expand-file-name "supercow-99" dir)))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(provide 'lice-tests)
;;; lice-tests.el ends here
