;;; cookbook.el --- produce cookbook pdf  -*- lexical-binding: t; -*-

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
