;;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, publish, blog
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:


;;; Requirements:

;; default renderer
(require 'org)      ; org-export-as
(require 'ox)       ; org-html-htmlize-output-type
(require 'cl-macs)  ; cl-defun


;;; Variables:


;; Internal:

(defvar peut-publier--regexp-nonwhitespace "[^[:space:]]"
  "Regular expression for non-whitespace character.

Meaning:

[^...]     complemented character set; match anything 'not these''
[:space:]  whitespace characters (see Info node `(elisp) Char Classes')

See Info node `(emacs) Regexps' for more details unless noted
otherwise.")

(defvar peut-publier--regexp-blank-line "^[[:blank:]]*$"
  "Regular expression for a line empty or full of whitespace.

Meaning:

^         beginning of line
[...]     character set
[:blank:] 'blank' characters (see Info node `(elisp) Char Classes')
*         zero or more of the preceding character
$         end of line

See Info node `(emacs) Regexps' for more details unless noted
otherwise.")

(defun peut-publier--org-id-filter (output &rest args)
  "Remove id tags from Org export OUTPUT string.

Org runs various filters defined within a backend on export.  A
filter requires three arguments: the code to be transformed, the
name of the back-end, and some optional information about the
export process.

ARGS catches the unused args and prevents flycheck from
complaining.

See `peut-publier--default-org-backend' for more details."
  (replace-regexp-in-string
   " id=\"[[:alpha:]-]*org[[:alnum:]]\\{7\\}\""
   ""
   output t))

(defvar peut-publier--default-org-backend
  (org-export-create-backend
   :name 'peut-publier
   :parent 'html
   :filters '((:filter-final-output . peut-publier--org-id-filter)))
  "Default export backend to use with `peut-publier-render-org-to-html'.

This is a workaround for Org idiosyncrasies.

The html Org export backend assigns random id attributes to tags.
This makes testing the output impossible.  This fix removes the
random attributes by attaching `peut-publier--id-filter' to a
custom backend.  It prevents
`org-export-filter-final-output-functions' from being polluted
and ensures `peut-publier--org-id-filter' is called only for
peut-publier related files.  If the default Org id becomes
testable, simply set this variable to 'html.

A typical solution would implement a hook.  The use of hooks,
however, would be redundant here.  A normal hook would require
setting up a temporary buffer in which to insert the rendered
text and to run the hooks.  This is precisely how the rendering
happens.  A non-normal hook, i.e. using `run-hook-with-args', is
basically what happens when the
`org-export-filter-final-output-functions' are applied.

See URL `https://emacs.stackexchange.com/a/52778/15177'
See Info node `(org) Advanced Export Configuration'.")

(defvar peut-publier--template-alist
  '((post . peut-publier-post-template))
  "Alist of publish templates.

The key is page type and the value the associated template.")


;; User:

(defvar peut-publier-static-head
  (concat "     <meta charset=\"UTF-8\">\n"
          "      <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
          "      <meta name=\"author\" content=\"Matt Trzcinski\">\n"
          "      <meta name=\"referrer\" content=\"no-referrer\">\n"
          "      <link href=\"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
          "      <link rel='shortcut icon' type=\"image/png\" href=\"static/favicon.png\" />\n")
  "Part of <head> which is always the same.")

(defun peut-publier-variable-head (page-path)
  "Part of <head> which varies depending on the PAGE-PATH."
  (concat
   "      <title>" (cdr (assoc "TITLE" (peut-publier-get-meta-data-alist page-path) 'string-equal)) "</title>\n"))

(defvar peut-publier-body-preamble
  (concat "   <div id=\"preamble\" class=\"status\">\n"
          "      <nav>\n"
          "         <div class=\"flexcontainer\">\n"
          "            <div class=\"smallitem\">\n"
          "               <ul class=\"inline-list\">\n"
          "                  <li>Excalamus</li>\n"
          "               </ul>\n"
          "            </div>\n"
          "            <div class=\"bigitem\">\n"
          "               <ul class=\"inline-list\">\n"
          "                  <li><a href=\".\">Home</a></li>\n"
          "                  <li><a href=\"about.html\">About</a></li>\n"
          "               </ul>\n"
          "            </div>\n"
          "         </div>\n"
          "      </nav>\n"
          "      <hr/>\n"
          "   </div>")
  "Section which appears above the content in <body>.")

(defvar peut-publier-body-postamble
  (concat "   <div id=\"postamble\" class=\"status\">\n"
          "      <hr/>\n"
          "      <p>Powered by <a href=\"https://github.com/excalamus/peut-publier\">peut-publier</a></p>\n"
          "      <p>©2020 Excalamus.com</p>\n"
          "    </div>\n")
  "Section which appears below the content in <body>.")

(defvar peut-publier-default-renderer #'peut-publier-render-org-to-html
  "Default renderer function.

Used with `peut-publier-render-to-html'.")

(defvar peut-publier-meta-data-start ""  ; beginning of file
  "Start delimiter for meta data relative to beginning of file.")

(defvar peut-publier-meta-data-end peut-publier--regexp-blank-line
  "End delimiter for meta data relative to beginning of file.")

(defvar peut-publier-default-meta-data-parser #'peut-publier-parse-org-meta-data
  "Default meta data parser.

Used with `peut-publier-get-meta-data-alist'.")


;;; Functions:


;; Meta data:
(defun peut-publier-strip-meta-data (file &optional start end)
  "Return contents of FILE with region between START and END removed.

Meta-data is the region between START and END.  Default START and
END correspond to `peut-publier-meta-data-start' and
`peut-publier-meta-data-end'."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let* ((start (or start (search-forward-regexp peut-publier-meta-data-start)))
           (end (or end (search-forward-regexp peut-publier-meta-data-end))))
      (delete-region start end)
      (buffer-string))))

(defun peut-publier-get-meta-data (file &optional start end)
  "Return meta-data between START and END in FILE.

Default START and END correspond to
`peut-publier-meta-data-start' and `peut-publier-meta-data-end'."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let* ((start (or start (search-forward-regexp peut-publier-meta-data-start)))
           (end (or end (search-forward-regexp peut-publier-meta-data-end))))
      (buffer-substring-no-properties start end))))

(defun peut-publier-parse-org-meta-data (data)
  "Parse Org formatted meta DATA into an alist.

Keywords are the '#+' options given within an Org file.  These
are things like TITLE, DATE, and FILETAGS.  Keywords are
case-sensitive!.  Values are whatever remains on that line."
  (with-temp-buffer
    (insert data)
    (org-element-map (org-element-parse-buffer 'element) 'keyword
      (lambda (x) (cons (org-element-property :key x)
                        (org-element-property :value x))))))

(defun peut-publier-get-meta-data-alist (file &optional parser)
  "Return FILE meta-data as alist using PARSER.

The `peut-publier-default-meta-data-parser' is used no PARSER is
provided."
  (let ((meta-data (peut-publier-get-meta-data file))
         (parser (or parser peut-publier-default-meta-data-parser)))
    (funcall parser meta-data)))


;; Render:

(cl-defun peut-publier-render-org-to-html (string &optional toc section-num (output-type 'css) (backend peut-publier--default-org-backend))
  "Convert STRING from Org syntax to html.

TOC and SECTION-NUM generate a table of contents and section
numbers, respectively.  Both default to nil.

OUTPUT-TYPE is used by the 'htmlize' package when formatting Org
source blocks for export.  Use 'css to export CSS selectors only,
'inline-css to export the CSS attribute values inline in the
HTML, or 'nil' to export source blocks as plain text.  Default is
'css.  See `org-html-htmlize-output-type'.

BACKEND is an `org-export-as' export backend.  Default is
`peut-publier--default-org-backend'.  While other values, such as
'latex may produce output, only html related backends are
supported by peut-publier.

\(fn STRING &optional TOC SECTION-NUM OUTPUT-TYPE BACKEND)"
;; 'toc'and 'sec-num' do not need a default value and can be nil,
;; 'output-type' needs a default and can be nil, 'backend' need
;; defaults and cannot be nil. See
;; https://emacs.stackexchange.com/questions/55684/
  (let* ((org-export-with-toc toc)
         (org-export-with-section-numbers section-num)
         (org-html-htmlize-output-type output-type)
         (backend (or backend peut-publier--default-org-backend)))
    (with-temp-buffer
      (insert string)
      (org-export-as backend nil nil t nil))))

(defun peut-publier-render-to-html (file &optional renderer &rest rargs)
  "Return html string conversion of FILE using RENDERER called with RARGS.

The default RENDERER is `peut-publier-default-renderer'.  The
user may provide their own RENDERER.  A RENDERER is a function
which accepts a file, returns an html string, and may accept
RARGS."
  (let ((renderer (or renderer peut-publier-default-renderer))
        (content (peut-publier-strip-meta-data file)))
    (apply renderer content rargs)))


;; Assemble:

(defun peut-publier-post-template (page-path)
  "Render PAGE-PATH as html string."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         (body-content (peut-publier-render-to-html page-path)))
    (concat
     "\n<div id=\"content\">\n"
     "<h1>" (cdr (assoc "TITLE" meta-data 'string-equal)) "</h1>\n"
     body-content
     "<div class=\"post-date\">"
     (cdr (assoc "DATE" meta-data 'string-equal))
     "</div>\n"
     "</div>\n")))

(defun peut-publier-assemble-page (page-path)
  "Assemble PAGE-PATH into final html string."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         ;; meta data values are strings; must convert type to symbol
         (type (intern (cdr (assoc "TYPE" meta-data 'string-equal))))
         (template (alist-get type peut-publier--template-alist))
         (page-content (funcall template page-path)))
    (when page-content
      (with-temp-buffer
        (insert (concat
                 "<!DOCTYPE html5>\n"
                 "<html lang=\"en\">\n"
                 "   <head>\n"
                 peut-publier-static-head
                 (peut-publier-variable-head page-path)
                 "   </head>\n"
                 "   <body>\n"
                 peut-publier-body-preamble
                 page-content
                 peut-publier-body-postamble
                 "   </body>\n"
                 "</html>"))
        (buffer-string)))))

(provide 'peut-publier)

;;; peut-publier.el ends here
