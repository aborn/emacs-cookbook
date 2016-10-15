;;; cookbook.el --- produce cookbook pdf  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (f "0.19.0") (s "1.10.0"))
;; Keywords: emacs cookbook
;; URL: https://github.com/aborn/emacs-cookbook

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; The package to build cookbook
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)

(defvar cookbook-root-dir "~/github/emacs-cookbook/")

(defun cookbook-org-async-batch-export-to-pdf ()
  "async do export to pdf"
  (interactive)
  (message "start export all org files to pdf formart.")
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      ,(async-inject-variables "\\`org-latex-pdf-process\\'")
      (require 'cookbook)
      (require 'cl-lib)
      (require 'f)
      (require 's)
      (cookbook-org-batch-export-to-pdf))
   (lambda (result)
     (message "%S" result)
     (message "finished export all org files to pdf formart."))))

(defun cookbook-org-batch-export-to-pdf ()
  "export all org file to pdf"
  (interactive)
  (let* ((files
          (cl-remove-if-not
           #'(lambda (x)
               (and (s-ends-with? ".org" x)
                    (not (s-ends-with? "README.org" x))))
           (f-files cookbook-root-dir))))
    (mapc #'(lambda (x)
              (let* ((src-file x)
                     (dest-file (concat
                                 (substring x 0 (- (length x) 3))
                                 "pdf")))
                (cookbook-org-to-pdf src-file)))
          files)))

(defun cookbook-org-to-pdf (src-file)
  "export source file to dest files"
  ;;(message "src-file: %s dest-file:%s" src-file dest-file)
  (find-file src-file)
  (org-latex-export-to-pdf))

(provide 'cookbook)
;;; cookbook.el ends here
