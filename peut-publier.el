;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, blog, publish
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:


;;; Requirements:

;; default renderer
(require 'org)      ; org-export-as
(require 'ox)       ; org-html-htmlize-output-type
(require 'cl-macs)  ; cl-defun


;;; Variables


;;;; Internal:

(defvar peut-publier--regexp-nonwhitespace "[^[:space:]]"
  "Regular expression for non-whitespace character.

Meaning:

[^...]     complemented character set; match anything 'not these''
[:space:]  whitespace characters (see Info node `(elisp) Char Classes')

See Info node `(emacs) Regexps' for more details unless noted
otherwise.")

(defvar peut-publier--regexp-blank-line "^[[:blank:]]$"
  "Regular expression for a line empty or full of whitespace.

Meaning:

^         beginning of line
[...]     character set
[:blank:] 'blank' characters (see Info node `(elisp) Char Classes')
*         zero or more of the preceding character
$         end of line

See Info node `(emacs) Regexps' for more details unless noted
otherwise.")


;;;; User:

(defvar peut-publier-default-renderer #'peut-publier-renderer-org-export
  "Default renderer function.

Used with `peut-publier-render-to-html'.")

(defvar peut-publier-meta-data-start ""  ; beginning of file
  "Start delimiter for meta data.")

(defvar peut-publier-meta-data-end peut-publier--regexp-blank-line
  "End delimiter for meta data.")


;;; Functions:

(defun peut-publier-strip-meta-data (file &optional start end)
  "Return contents of FILE with meta-data removed.

Meta-data is the region between START and END.  Defaults are
`peut-publier-meta-data-start' and `peut-publier-meta-data-end'."
  (let* (())

    )
)

(cl-defun peut-publier-renderer-org-export (file &optional toc section-num (output-type 'css) (backend 'html))
  "Convert Org FILE to html string.

TOC and SECTION-NUM generate a table of contents and section
numbers, respectively.  Both default to nil.

OUTPUT-TYPE is used by the 'htmlize' package when formatting Org
source blocks for export.  Use 'css to export CSS selectors only,
'inline-css to export the CSS attribute values inline in the
HTML, or 'nil' to export source blocks as plain text.  Default is
'css.  See `org-html-htmlize-output-type'.

BACKEND is an `org-export-as' export backend.  Default is 'html.
While other values, such as 'latex may produce output, only html
related backends are supported by peut-publier.

\(fn FILE &optional TOC SECTION-NUM OUTPUT-TYPE BACKEND)"
;; 'toc'and 'sec-num' do not need a default value and can be nil,
;; 'output-type' needs a default and can be nil, 'backend' need
;; defaults and cannot be nil. See
;; https://emacs.stackexchange.com/questions/55684/
  (let* ((org-export-with-toc toc)
	 (org-export-with-section-numbers section-num)
	 (org-html-htmlize-output-type output-type)
	 (backend (or backend 'html)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (org-export-as backend nil nil t nil))))

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
