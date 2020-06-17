;; This file demonstrates using peut-publier with a different
;; rendering engine.  It is not included in the tests because it
;; relies on an external application.  It imitates, in an admittedly
;; convoluted way, how a person could configure peut-publier in their
;; init.

(defvar peut-publier-demo-md-content
  (concat
   "# Header\n"
   "## Subheader\n"
   "Hello, world!\n\n"
   "```\n"
   "print('Goodbye, cruel world...')\n"
   "```\n")
  "Sample post file written in markdown.")

(defvar peut-publier-demo-expected-output
  (concat
   "<h1>Header</h1>\n"
   "\n"
   "<h2>Subheader</h2>\n"
   "\n"
   "<p>Hello, world!</p>\n"
   "\n"
   "<p><code>\n"
   "print(&#39;Goodbye, cruel world...&#39;)\n"
   "</code></p>\n")
  "Expected html string after rendering.")

;; create a user-defined renderer
(defun peut-publier-demo-md-renderer (file)
 "Convert markdown FILE as html."
 (shell-command-to-string (concat "hoedown " file)))

;; redefine the peut-publier-default-renderer as a user defined
;; markdown engine.  In real life, this would simply be done with:
;;
;; (setq peut-publier-default-renderer #'peut-publier-demo-md-renderer)
(let* ((test-file (concat (temporary-file-directory) "test-file"))
        (peut-publier-default-renderer #'peut-publier-demo-md-renderer)
        (result (progn
                (with-temp-file test-file
                    (insert peut-publier-demo-md-content))
                (peut-publier-render-to-html test-file))))
  (delete-file test-file)
  (if (string-match-p result peut-publier-demo-expected-output)
      (message "It rendered as expected!")
    (message "Things didn't work :(")))
