;;; cookbook.el --- produce cookbook pdf  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (f "0.19.0") (s "1.10.0") (async "1.9"))
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
(require 'async)

(defvar cookbook-root-dir "~/github/emacs-cookbook/")
(defvar cookbook-name "emacs-cookbook")

(defgroup cookbook nil
  "cookbook group"
  :prefix "cookbook-"
  :group 'org-export-pdf)

(defcustom cookbook-chapters-dir
  (expand-file-name "chapters" cookbook-root-dir)
  "Chapters root path"
  :type 'string
  :group 'cookbook)

(defun cookbook-chapter-org-files ()
  "Get all chater org files."
  (cl-remove-if-not
   #'(lambda (x)
       (and (s-ends-with? ".org" x)
            (not (s-ends-with? "README.org" x))))
   (f-files cookbook-chapters-dir)))

(defun cookbook-run-async ()
  "Do cookbook-run actioin use async method. 异步生成emacs-cookbook.pdf"
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
      (cookbook-run))
   (lambda (result)
     (message "%S" result)
     (message "finished export all org files to pdf formart."))))

(defun cookbook-run ()
  "Export all org file to pdf."
  (interactive)
  (let* ((files (cookbook-chapter-org-files)))
    (mapc #'(lambda (x)
              (let* ((src-file x)
                     (dest-file (concat
                                 (substring x 0 (- (length x) 3))
                                 "pdf")))
                (cookbook-org-to-pdf src-file)))
          files)
    (cookbook-produce)))

(defun cookbook-org-to-pdf (src-file)
  "export source file to dest files"
  ;;(message "src-file: %s dest-file:%s" src-file dest-file)
  (find-file src-file)
  (org-latex-export-to-pdf))

(defun cookbook-header-content ()
  (concat
   "#+TITLE: emacs实践笔记\n"
   "#+AUTHOR: aborn\n"
   (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M" (current-time)))
   "#+EMAIL: aborn.jiang@gmail.com\n"
   "#+LANGUAGE: zh\n"
   "#+LATEX_HEADER: \\usepackage{xeCJK}\n\n"
   "#+SETUPFILE: ~/github/org-html-themes/setup/theme-readtheorg.setup"
   "-----\n"))

(defun cookbook-org-content-extract ()
  "Extract current buffer org content (hearder content excluded.)"
  (let* ((content (buffer-string))
         (s-begin (string-match "-----\n" content)))
    (when s-begin
      (substring-no-properties content (+ s-begin 5)))))

(defun cookbook-produce-text-content ()
  (concat
   (cookbook-header-content)
   (cl-reduce 'concat
              (mapcar #'(lambda (org-file)
                          (find-file org-file)
                          (cookbook-org-content-extract))
                      (cookbook-chapter-org-files)))))

(defun cookbook-org-file-name ()
  (expand-file-name
   (concat cookbook-name ".org")
   cookbook-root-dir))

(defun cookbook-produce ()
  "Produce cookbook org & pdf."
  (interactive)
  (let* ((cookbook-content (cookbook-produce-text-content))
         (cookbook-file (cookbook-org-file-name)))
    (find-file cookbook-file)
    (erase-buffer)
    (insert cookbook-content)
    (save-buffer)
    (cookbook-org-to-pdf cookbook-file)))

(provide 'cookbook)
;;; cookbook.el ends here
