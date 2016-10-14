
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
    (setq ab/debug files)))

(provide 'cookbook)
