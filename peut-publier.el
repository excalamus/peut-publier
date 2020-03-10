;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, blog, publish
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:

(require 'org)
(require 'ox) ;; org-html-htmlize-output-type

(defun peut-publier-renderer-org-export (file &optional toc section-num output-type backend)
  "Export FILE to html string using `org-export-as'.

This function wraps `org-export-as'.  See that function for more
details about the arguments.

TOC and SECTION-NUM generate table of contents and section
numbers, respectively.  Defaults for each are nil.

OUTPUT-TYPE is 'css, 'inline-css, or nil as defined by
`org-html-htmlize-output-type'.  Default is 'css.

BACKEND is the export backend.  Default is 'html."
  (let* ((org-export-with-toc toc)
	 (org-export-with-section-numbers section-num)
	 ;; To toggle nil, user must specify 'plain-text
	 (org-html-htmlize-output-type
	  (cond ((eq output-type 'css) output-type)
		((eq output-type 'inline-css) output-type)
		((eq output-type 'plain-text) nil)
		('css)))
	 (backend (or backend 'html)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (org-export-as backend nil nil t nil))))

(defvar peut-publier-default-renderer #'peut-publier-renderer-org-export
  "Default renderer function to be used with
`peut-publier-render-to-html'.")

(defun peut-publier-render-to-html (file &optional renderer &rest rargs)
  "Return html string conversion of FILE using RENDERER called with RARGS.

The default RENDERER is `peut-publier-default-renderer'.  The
user may provide their own RENDERER.  A RENDERER is a function
which accepts a file, returns an html string, and may accept
RARGS."
  (let ((renderer (or renderer peut-publier-default-renderer)))
    (apply renderer file rargs)))

(provide 'peut-publier)

;;; peut-publier.el ends here
