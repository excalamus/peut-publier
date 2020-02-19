;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'peut-publier)

;; "Sample post file without meta information."
(setq peut-publier-test-post-content
      (concat "Hello, world!\n"))

(ert-deftest peut-publier-render-to-html ()
  "Test the main rendering function."
  (let ((temp-file (concat (temporary-file-directory) "dummy-post")))
    (with-temp-file temp-file
      (insert peut-publier-test-post-content))
      (should (eq (type-of (peut-publier-render-to-html temp-file)) 'string))))

(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
