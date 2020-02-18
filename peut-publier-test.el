;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'peut-publier)
(require 'ert)

;; "Sample post file without meta information."
(setq peut-publier-test-post-content
      (concat "Hello, world!"))

(ert-deftest peut-publier-render-to-html ()
  "Test the main rendering function."
  (should (eq (type-of (peut-publier-render-to-html)) 'string))

(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
