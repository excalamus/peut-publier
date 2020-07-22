;;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, publish, blog
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;; The peut-publier namespace is verbose.  For improved readability,
;; use `nameless-mode': https://github.com/Malabarba/Nameless

;;; Code:


;;; Requirements:

;; default renderer
(require 'org)      ; org-export-as
(require 'ox)       ; org-html-htmlize-output-type
(require 'cl-macs)  ; cl-defun

;; peut-publier-dir-list
(require 'seq)


;;; Variables:

(defvar peut-publier-lml nil
  "Lightweight markup language.")

(defvar peut-publier-root-directory nil
  "Top level directory of website.")

(defvar peut-publier-src-directory nil
  "Directory where page source files are stored.")

(defvar peut-publier-publish-directory nil
  "Directory where converted files are published.")

(defvar peut-publier-author nil
  "Author name.")

(defvar peut-publier-site-name nil
  "Website name.")

(defvar peut-publier-tld nil
  "Top level domain name for site.")

(defvar peut-publier-about-img nil
  "About page picture.")

(defvar peut-publier-about-img-alt nil
  "About page picture alt text.")

(defvar peut-publier-meta-data-start nil
  "Start delimiter for meta data relative to beginning of file.")

(defvar peut-publier-meta-data-end nil
  "End delimiter for meta data relative to beginning of file.")

(defvar peut-publier-default-meta-data-parser nil
  "Default meta data parser.

A meta-data parser is a function which accepts a string and
returns an alist.")

(defvar peut-publier-static-head nil
  "HTML in <head> which is always the same.")

(defvar peut-publier-variable-head nil
  "HTML in <head> which changes depending on page.

The variable head is provided by a function which accepts a page
path and returns an HTML string.")

(defvar peut-publier-body-preamble nil
  "HTML which appears above the content in <body>.")

(defvar peut-publier-body-postamble nil
  "HTML which appears below the content in <body>.")

(defvar peut-publier-default-renderer nil
  "Default renderer function.

A renderer is a function which accepts a file, returns an HTML
string, and may accept args.")


;;; Internal

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

(defvar peut-publier--default-org-backend nil
  "Default export backend to use with `peut-publier-render-org-to-html'.

This is a workaround for Org idiosyncrasies.

The HTML Org export backend assigns random id attributes to tags.
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

(defvar peut-publier--template-alist nil
  "Alist of publish templates.

Key is page type (symbol) and value the associated template
function.")

(defvar peut-publier--index-exclude nil
  "List of files to exclude from the main index.")


;;; Functions:

(defun peut-publier--org-id-filter (output &rest _args)
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

(defun peut-publier-alist-get (key alist &optional default remove)
  "Get value associated with KEY in ALIST using `string-equal'.

Wrapper for `alist-get'.  See `alist-get' for explanation of
DEFAULT and REMOVE.

Meta-data is read from file and parsed into an alist with string
keys.  Alist tooling doesn't play well with string keys.
Converting keys to symbols also presents issues (e.g. illegal
characters).  This function is a compromise between the two
situation."
  (alist-get key alist default remove 'string-equal))

(defun peut-publier-relative-to (dir path &optional extension)
  "Return the PATH modified relative to DIR.

Optionally change extension to EXTENSION."
  (let ((relative-path (concat
                        (file-name-as-directory dir)
                        (file-name-nondirectory path))))
    (if extension
        (concat (file-name-sans-extension relative-path) extension)
      relative-path)))

(defun peut-publier-dir-list (dir &optional include exclude)
  "Return list of files in DIR.

INCLUDE files matching regexp.  EXCLUDE names containing.
Exclude happens after include."
  (let ((dir-list (mapcar (lambda (x) (concat dir x))
                          (directory-files dir nil include))))
    (if exclude
        (seq-remove
         (lambda (x) (string-match-p (regexp-opt exclude) x))
         dir-list)
      dir-list)))

(defun peut-publier-html-page-title (page-path)
  "Create HTML title for PAGE-PATH."
  (concat
   "      <title>" (peut-publier-alist-get "TITLE" (peut-publier-get-meta-data-alist page-path)) "</title>\n"))

(defun peut-publier-html-page-list (&optional dir)
  "Return HTML list of pages in DIR."
  (let ((dir (or dir peut-publier-src-directory)))
    (mapconcat
     (lambda (page-path)
       (concat
        "   <li><p class=\"post-title\"><a href=\""
        (peut-publier-relative-to peut-publier-tld page-path ".html") "\">"
        (peut-publier-alist-get "TITLE" (peut-publier-get-meta-data-alist page-path))
        "</a></p></li>\n"))
     (peut-publier-dir-list dir (concat "\\." peut-publier-lml "$") peut-publier--index-exclude)
     "")))


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
  "Convert STRING from Org syntax to HTML.

TOC and SECTION-NUM generate a table of contents and section
numbers, respectively.  Both default to nil.

OUTPUT-TYPE is used by the 'htmlize' package when formatting Org
source blocks for export.  Use 'css to export CSS selectors only,
'inline-css to export the CSS attribute values inline in the
HTML, or 'nil' to export source blocks as plain text.  Default is
'css.  See `org-html-htmlize-output-type'.

BACKEND is an `org-export-as' export backend.  Default is
`peut-publier--default-org-backend'.  While other values, such as
'latex may produce output, only HTML related backends are
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

(defun peut-publier-render-to-html (file &optional renderer &rest args)
  "Return HTML string conversion of FILE using RENDERER called with ARGS.

The default RENDERER is `peut-publier-default-renderer'.  The
user may provide their own RENDERER.  A RENDERER is a function
which accepts a file, returns an HTML string, and may accept
ARGS."
  (let ((renderer (or renderer peut-publier-default-renderer))
        (content (peut-publier-strip-meta-data file)))
    (apply renderer content args)))


;; Assemble:

(defun peut-publier-post-template (page-path)
  "Insert PAGE-PATH into HTML string template."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         (toc (peut-publier-alist-get "TOC" meta-data))
         (body-content (peut-publier-render-to-html page-path peut-publier-default-renderer toc)))
    (concat
     "\n<div id=\"content\">\n"
     "<h1>" (peut-publier-alist-get "TITLE" meta-data) "</h1>\n"
     body-content
     "<div class=\"post-date\">"
     (peut-publier-alist-get "DATE" meta-data)
     "</div>\n"
     "</div>\n")))

(defun peut-publier-about-template (page-path)
  "Insert PAGE-PATH into HTML string template."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         (body-content (peut-publier-render-to-html page-path)))
            (concat
             "<div id=\"content\">\n"
             "<img id=\"img-float\" src=\""
             peut-publier-about-img "\" alt=\""
             peut-publier-about-img-alt "\">\n"
             "<h1 class=\"title\">" (peut-publier-alist-get "TITLE" meta-data) "</h1>\n"
             body-content
             "</div>\n")))

(defun peut-publier-index-template (page-path)
  "Insert PAGE-PATH into HTML string template."
  (let* ((body-content (peut-publier-render-to-html page-path)))
    (concat
     "<div id=\"content\">\n"
     body-content
     "<ul>\n"
     (peut-publier-html-page-list peut-publier-src-directory)
     "</ul>\n"
     "</div>\n")))


;; Publish:

(defun peut-publier-assemble-page (page-path)
  "Assemble PAGE-PATH into final HTML string."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         (mtype (peut-publier-alist-get "TYPE" meta-data))
         (type (cond ((stringp mtype) (intern mtype))    ; convert strings to symbols
                     ((and (symbolp mtype) mtype) mtype) ; not nil
                     (t 'post)))                         ; default is 'post
         (template (peut-publier-alist-get type peut-publier--template-alist))
         (page-content (funcall template page-path)))
    (when page-content
      (with-temp-buffer
        (insert (concat
                 "<!DOCTYPE html5>\n"
                 "<html lang=\"en\">\n"
                 "   <head>\n"
                 peut-publier-static-head
                 (funcall peut-publier-variable-head page-path)
                 "   </head>\n"
                 "   <body>\n"
                 peut-publier-body-preamble
                 page-content
                 peut-publier-body-postamble
                 "   </body>\n"
                 "</html>"))
        (buffer-string)))))

(defun peut-publier-publish-page (page-path &optional out-dir)
  "Publish PAGE-PATH as HTML file in OUT-DIR.

The `peut-publier-publish-directory' is used when no OUT-DIR is
given."
  (let* ((out-dir (or out-dir peut-publier-publish-directory))
         (out-file (peut-publier-relative-to out-dir page-path ".html")))
    (condition-case nil
        (progn
          (with-temp-file out-file
            (insert (peut-publier-assemble-page page-path)))
          (message "Wrote %s" out-file))
      (error "Could not publish %s" page-path))))

(defun peut-publier-publish (&optional list out-dir)
  "Publish pages.

LIST is a list of absolute paths to page sources.  If no list is
given, all files in the `peut-publier-src-directory' will be
used.

Unless OUT-DIR, publish pages to
`peut-publier-publish-directory'."
  (interactive)
  (let ((list (or list (peut-publier-dir-list peut-publier-src-directory)))
        (out-dir (or out-dir peut-publier-publish-directory)))
    (mapc (lambda (x)
            (funcall  #'peut-publier-publish-page x out-dir))
          list)
    (message "Site rendered successfully")))


;;; Defaults:

(setq peut-publier-lml "org")

(setq peut-publier-root-directory "~/site/")
(setq peut-publier-src-directory "~/site/src/")
(setq peut-publier-publish-directory "~/site/publish/")
(setq peut-publier-author "Excalamus")
(setq peut-publier-site-name "peut-pubier")
(setq peut-publier-tld ".")
(setq peut-publier-about-img "static/about.jpg")
(setq peut-publier-about-img-alt "Headshot")

(setq peut-publier-meta-data-start "")  ; beginning of file
(setq peut-publier-meta-data-end peut-publier--regexp-blank-line)
(setq peut-publier-default-meta-data-parser #'peut-publier-parse-org-meta-data)

(setq peut-publier-static-head
  (concat "     <meta charset=\"UTF-8\">\n"
          "      <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
          "      <meta name=\"author\" content=\"" peut-publier-author "\">\n"
          "      <meta name=\"referrer\" content=\"no-referrer\">\n"
          "      <link href=\"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
          "      <link rel='shortcut icon' type=\"image/png\" href=\"static/favicon.png\" />\n"))

(setq peut-publier-variable-head #'peut-publier-html-page-title)

(setq peut-publier-body-preamble
  (concat
   "   <div id=\"preamble\" class=\"status\">\n"
   "      <nav>\n"
   "         <div class=\"flexcontainer\">\n"
   "            <div class=\"smallitem\">\n"
   "               <ul class=\"inline-list\">\n"
   "                  <li><a href=\"index.html\" id=\"website\">" peut-publier-site-name "</a></li>\n"
   "               </ul>\n"
   "            </div>\n"
   "            <div class=\"bigitem\">\n"
   "               <ul class=\"inline-list\">\n"
   "                  <li><a href=\"about.html\">About</a></li>\n"
   "               </ul>\n"
   "            </div>\n"
   "         </div>\n"
   "      </nav>\n"
   "      <hr/>\n"
   "   </div>\n"))

(setq peut-publier-body-postamble
  (concat "   <div id=\"postamble\" class=\"status\">\n"
          "      <hr/>\n"
          "      <p>Powered by <a href=\"https://github.com/excalamus/peut-publier\">peut-publier</a></p>\n"
          "      <p>©" (format-time-string "%Y") " " peut-publier-site-name "</p>\n"
          "    </div>\n"))

(setq peut-publier-default-renderer #'peut-publier-render-org-to-html)

(setq peut-publier--default-org-backend
  (org-export-create-backend
   :name 'peut-publier
   :parent 'html
   :filters '((:filter-final-output . peut-publier--org-id-filter))))

(setq peut-publier--template-alist
  '((post . peut-publier-post-template)
    (about . peut-publier-about-template)
    (index . peut-publier-index-template)))

(setq peut-publier--index-exclude '("index" "about"))

(provide 'peut-publier)

;;; peut-publier.el ends here
