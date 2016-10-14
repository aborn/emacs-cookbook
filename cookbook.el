
(defvar cookbook-root-dir "~/github/emacs-cookbook/")
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
                (cookbook-org-to-pdf src-file dest-file)))
          files)))

(defun cookbook-org-to-pdf ()
  "export source file to dest files"
  ;;(message "src-file: %s dest-file:%s" src-file dest-file)
  (find-file "/Users/aborn/github/emacs-cookbook/org.org")
  (org-latex-export-to-pdf))

(provide 'cookbook)
