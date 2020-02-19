;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'peut-publier)

;; "Sample post file without meta information."
(setq peut-publier-test-post-content
      (concat "* Header\n"
              "** header2\n"
              "Hello, world!\n"))

(ert-deftest peut-publier-render-to-html ()
  "Test the main rendering function."
  (let ((temp-file (concat (temporary-file-directory) "dummy-post")))
    (with-temp-file temp-file
      (insert peut-publier-test-post-content))
      (should (eq (type-of (peut-publier-render-to-html temp-file)) 'string))))

(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
(peut-publier-render-to-html my-file t t)

(org-export-as 'html nil nil t nil)
