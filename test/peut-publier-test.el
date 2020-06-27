;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(require 'ert)
(require 'peut-publier)


;;; Variables:

(defvar peut-publier-test-post-meta-data
  (concat
   "#+TITLE: Test post\n"
   "#+AUTHOR: Excalamus\n"
   "#+TAGS: blogging tests\n"
   "\n")
  "Sample post meta information.")

(defvar peut-publier-test-post-content
  (concat
   "* Header\n"
   "** Subheader\n"
   "Hello, world!\n\n"
    ;; Using #+begin_src creates inline-css, whereas #+begin_example
    ;; does not
   "#+begin_src python\n"
   "    print('Goodbye, cruel world...')\n"
   "#+end_src\n")
  "Sample post file without meta information.")

(defvar peut-publier-test-post
  (concat
   peut-publier-test-post-meta-data
   peut-publier-test-post-content)
  "Sample post file.")

;;; Tests:


;;;; Renderer

(ert-deftest peut-publier-test-render-to-html ()
  "Test that `peut-publier-render-to-html' converts a file to
html string.  Uses `peut-publier-default-renderer'."
(let* ((test-file (concat (temporary-file-directory) "test-file"))
       (result (progn
                 (with-temp-file test-file
                   (insert peut-publier-test-post-content))
                 (peut-publier-render-to-html test-file))))
  (delete-file test-file)
    (should (string-match-p (regexp-quote "</div>") result))))

(ert-deftest peut-publier-test-render-org-to-html-toc-nil ()
  "Test that `peut-publier-render-org-to-html' does not output
table of contents unless user passes t."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil)))
    (should-not (string-match-p "id=\"table-of-contents\"" result))))

(ert-deftest peut-publier-test-render-org-to-html-toc-t ()
  "Test that `peut-publier-render-org-to-html' outputs table of
contents when t."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post t)))
    (should (string-match-p "id=\"table-of-contents\"" result))))

(ert-deftest peut-publier-test-render-org-to-html-section-num-nil ()
  "Test that `peut-publier-render-org-to-html' does not output
section numbers unless user passes t."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil nil)))
    (should-not (string-match-p "class=\"section-number" result))))

(ert-deftest peut-publier-test-render-org-to-html-section-num-t ()
  "Test that `peut-publier-render-org-to-html' does not output
section numbers unless user passes t."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil t)))
    (should (string-match-p "class=\"section-number" result))))

(ert-deftest peut-publier-test-render-org-to-html-inline-css ()
  "Test that `peut-publier-render-org-to-html' output type 'inline-css
generates inline-css."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil nil 'inline-css)))
    (should (string-match-p (regexp-quote "span style=") result))))

(ert-deftest peut-publier-test-render-org-to-html-css ()
  "Test that `peut-publier-render-org-to-html' output type 'css
does not generate inline-css."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil nil 'css)))
    (should-not (string-match-p (regexp-quote "span style=") result))))

(ert-deftest peut-publier-test-render-org-to-html-plain-text ()
  "Test that `peut-publier-render-org-to-html' output type nil
does not generate style ('inline-css) or class ('css) span tags."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil nil nil)))
    (should-not (string-match-p (regexp-quote "span style=") result))
    (should-not (string-match-p (regexp-quote "span class=") result))))

(ert-deftest peut-publier-test-render-org-to-html-default-backend ()
  "Test that `peut-publier-render-org-to-html' default backend
is 'html."
  (let* ((result (peut-publier-render-org-to-html peut-publier-test-post nil nil nil nil)))
    (should (string-match-p (regexp-quote "</div>") result))))


;;; Meta-data

(ert-deftest peut-publier-test-strip-meta-data ()
  "Test that meta-data is removed and the content returned."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-strip-meta-data test-file)))
    (delete-file test-file)
    (should (string-equal peut-publier-test-post-content result)))))

(ert-deftest peut-publier-test-get-meta-data ()
  "Test that meta-data is returned."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post-content))
                   (peut-publier-get-meta-data test-file)))
    (delete-file test-file)
    (should (string-equal peut-publier-test-post-meta-data result)))))

(ert-deftest peut-publier-test-parse-org-meta-data ()
  "Test that Org meta-data is correctly parsed into an alist."
  (let ((result (peut-publier-parse-org-meta-data
                 peut-publier-test-post-meta-data)))
    (should (equal '(("TITLE" . "Test post")
                     ("AUTHOR" . "Excalamus")
                     ("TAGS" . "blogging tests"))
                   result))))

(ert-deftest peut-publier-test-get-keyword-value ()
  "Test that values can be extracted from meta-data via key."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post))
                   (peut-publier-get-keyword-value "TITLE" test-file)))
    (delete-file test-file)
    (should (string-equal "Test post" result)))))


(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
