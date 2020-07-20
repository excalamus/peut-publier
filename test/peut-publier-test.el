;;; peut-publier-test.el --- test suite for peut-publier.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The peut-publier-test namespace is verbose.  For improved readability, use
;; `nameless-mode': https://github.com/Malabarba/Nameless


;;; Code:

(require 'ert)
(require 'peut-publier)


;;; Variables:

(defvar peut-publier-test-post-meta-data
  (concat
   "#+TITLE: Test post\n"
   "#+AUTHOR: Excalamus\n"
   "#+DATE: 2020-07-17\n"
   "#+TAGS: blogging tests\n"
   "#+TYPE: post\n"
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

(defvar peut-publier-test-assembled-post
  (concat "<!DOCTYPE html5>\n"
          "<html lang=\"en\">\n"
          "   <head>\n"
          peut-publier-static-head
          ;; peut-publier-variable-head
          "      <title>Test post</title>\n"
          ;;
          "   </head>\n"
          "   <body>\n"
          peut-publier-body-preamble
          ;; page content
          "\n<div id=\"content\">\n"
          "<h1>Test post</h1>\n"
          "<div class=\"outline-2\">\n"
          "<h2>Header</h2>\n"
          "<div class=\"outline-text-2\">\n"
          "</div>\n"
          "<div class=\"outline-3\">\n"
          "<h3>Subheader</h3>\n"
          "<div class=\"outline-text-3\">\n"
          "<p>\n"
          "Hello, world!\n"
          "</p>\n"
          "\n"
          "<div class=\"org-src-container\">\n"
          "<pre class=\"src src-python\"><span class=\"org-keyword\">print</span>(<span class=\"org-string\">'Goodbye, cruel world...'</span>)\n"
          "</pre>\n"
          "</div>\n"
          "</div>\n"
          "</div>\n"
          "</div>\n"
          "<div class=\"post-date\">2020-07-17</div>\n"
          "</div>\n"
          peut-publier-body-postamble
          ;;
          "   </body>\n"
          "</html>")
  "Post after being assembled.")


;;; Tests:

(ert-deftest peut-publier-test-alist-get ()
  "Test alist-get wrapper."
  (let* ((alist '(("TITLE" . "Test post")
                 ("AUTHOR" . "Excalamus")
                 ("DATE" . "2020-07-17")
                 ("TAGS" . "blogging tests")))
         (result (alist-get "TAGS" alist nil t 'string-equal)))
    ;; test that strings are used as keys
    (should (string-equal "Test post" (peut-publier-alist-get "TITLE" alist)))
    ;; test that default return value can be set
    (should (eql 'post (peut-publier-alist-get "TYPE" alist 'post)))))


;; Render:

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


;; Meta data:

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
                     ("DATE" . "2020-07-17")
                     ("TAGS" . "blogging tests")
                     ("TYPE" . "post"))
                   result))))

(ert-deftest peut-publier-test-meta-data-alist ()
  "Test that meta-data is correctly parsed into an alist."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post))
                   (peut-publier-get-meta-data-alist test-file))))
    (delete-file test-file)
    (should (equal '(("TITLE" . "Test post")
                     ("AUTHOR" . "Excalamus")
                     ("DATE" . "2020-07-17")
                     ("TAGS" . "blogging tests")
                     ("TYPE" . "post"))
                   result))))


;; Assemble:

(ert-deftest peut-publier-test-assemble-page ()
  "Test that pages are properly assembled."
  (let* ((test-file (concat (temporary-file-directory) "test-file"))
         (result (progn
                   (with-temp-file test-file
                     (insert peut-publier-test-post))
                   (peut-publier-assemble-page test-file))))
    (delete-file test-file)
    (should (string-equal peut-publier-test-assembled-post result))))


;; Publish:

(ert-deftest peut-publier-test-relative-to ()
  "Test relative path modifications."
  (let* ((path "/home/foo/site/src/test-post.org")
         (dir "/home/foo/site/publish")
         (result (peut-publier-relative-to dir path ".html")))
    (should (string-equal "/home/foo/site/publish/test-post.html" result))))

(ert-deftest peut-publier-test-publish-page ()
  "Test that page publishes to file."
  (let* ((filename (concat (temporary-file-directory) "test-file"))
         (post (concat filename ".org"))
         (published (concat filename ".html"))
         (result (progn
                   (with-temp-file post
                     (insert peut-publier-test-post))
                   (peut-publier-publish-page post (temporary-file-directory))
                   (with-temp-buffer
                     ;; when insert-file-contents-literally is used,
                     ;; copyright symbol is given in UTF-8 Octal Bytes
                     (insert-file-contents published)
                     (buffer-string)))))
    (delete-file post)
    (delete-file published)
    (should (string-equal peut-publier-test-assembled-post result))))

(provide 'peut-publier-test)

;;; peut-publier-test.el ends here
