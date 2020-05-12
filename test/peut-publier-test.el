;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'peut-publier)

;;; Variables:

(defvar peut-publier-test-post-content
  (concat
   "* Header\n"
   "** Subheader\n"
   "Hello, world!\n\n"
    ;; #+begin_src creates inline-css,
    ;; #+begin_example does not
   "#+begin_src python\n"
   "    print('Goodbye, cruel world...')\n"
   "#+end_src\n")
  "Sample post file without meta information.")

;;; Tests:

(ert-deftest peut-publier-test-render-to-html ()
  "Test that `peut-publier-render-to-html' accepts a file and
returns a string.  Use `peut-publier-default-renderer'."
(let* ((test-file (concat (temporary-file-directory) "test-file"))
       (result (progn
                 (with-temp-file test-file
                   (insert peut-publier-test-post-content))
                 (peut-publier-render-to-html test-file))))
  (delete-file test-file)
  (should (eq (type-of result) 'string))))

(ert-deftest peut-publier-test-renderer-org-export-toc-nil ()
  "Test that `peut-publier-renderer-org-export' does not output
table of contents unless user passes t."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil))))
    (delete-file test-file)
    (should-not (string-match-p "id=\"table-of-contents\"" result))))

(ert-deftest peut-publier-test-renderer-org-export-toc-t ()
  "Test that `peut-publier-renderer-org-export' outputs table of
contents when t."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file t))))
    (delete-file test-file)
    (should (string-match-p "id=\"table-of-contents\"" result))))

(ert-deftest peut-publier-test-renderer-org-export-section-num-nil ()
  "Test that `peut-publier-renderer-org-export' does not output
section numbers unless user passes t."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil nil))))
    (delete-file test-file)
    (should-not (string-match-p "class=\"section-number" result))))

(ert-deftest peut-publier-test-renderer-org-export-section-num-t ()
  "Test that `peut-publier-renderer-org-export' does not output
section numbers unless user passes t."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil t))))
    (delete-file test-file)
    (should (string-match-p "class=\"section-number" result))))

(ert-deftest peut-publier-test-renderer-org-export-inline-css ()
  "Test that `peut-publier-renderer-org-export' output type 'inline-css
generates inline-css."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     ;; #+begin_src creates inline-css,
                     ;; #+begin_example does not
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil nil 'inline-css))))
    (delete-file test-file)
    (should (string-match-p (regexp-quote "span style=") result))))

(ert-deftest peut-publier-test-renderer-org-export-css ()
  "Test that `peut-publier-renderer-org-export' output type 'css
does not generate inline-css."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     ;; #+begin_src creates inline-css,
                     ;; #+begin_example does not
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil nil 'css))))
    (delete-file test-file)
    (should-not (string-match-p (regexp-quote "span style=") result))))

(ert-deftest peut-publier-test-renderer-org-export-plain-text ()
  "Test that `peut-publier-renderer-org-export' output type nil
does not generate style ('inline-css) or class ('css) span tags."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     ;; #+begin_src creates inline-css,
                     ;; #+begin_example does not
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil nil nil))))
    (delete-file test-file)
    (should-not (string-match-p (regexp-quote "span style=") result))
    (should-not (string-match-p (regexp-quote "span class=") result))))

(ert-deftest peut-publier-test-renderer-org-export-default-backend ()
  "Test that `peut-publier-renderer-org-export' default backend
is 'html."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-renderer-org-export test-file nil nil nil nil))))
    (delete-file test-file)
    (should (string-match-p (regexp-quote "</div>") result))))

(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
