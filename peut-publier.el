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

(defgroup peut-publier nil
  "Extensible static website generator."
  :prefix "peut-publier-"
  :group 'applications)

(defcustom peut-publier-lml nil
  "Lightweight markup language."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-root-directory nil
  "Top level directory of website."
  :type 'directory
  :safe 'file-directory-p
  :group 'peut-publier)

(defcustom peut-publier-src-directory nil
  "Directory where page source files are stored."
  :type 'directory
  :safe 'file-directory-p
  :group 'peut-publier)

(defcustom peut-publier-publish-directory nil
  "Directory where converted files are published."
  :type 'directory
  :safe 'file-directory-p
  :group 'peut-publier)

(defcustom peut-publier-author nil
  "Author name."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-site-name nil
  "Website name."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-tld nil
  "Top level domain name for site."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-about-img nil
  "Path to the \"About\" page picture.

Must be given relative to `peut-publier-publish-directory'."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-about-img-alt nil
  "Alt text for `peut-publier-about-img'."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-meta-data-start nil
  "Start delimiter for meta data relative to beginning of file.

Must be a regexp."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-meta-data-end nil
  "End delimiter for meta data relative to beginning of file.

Must be a regexp."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-default-meta-data-parser nil
  "Default meta data parser.

A meta-data parser is a function which accepts a string and
returns an alist."
  :type 'function
  :safe 'functionp
  :group 'peut-publier)

(defcustom peut-publier-static-head nil
  "HTML in <head> which is always the same."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-variable-head nil
  "HTML in <head> which changes depending on page.

The variable head is provided by a function which accepts a page
path and returns an HTML string."
  :type 'function
  :safe 'functionp
  :group 'peut-publier)

(defcustom peut-publier-body-preamble nil
  "HTML which appears above the content in <body>."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-body-postamble nil
  "HTML which appears below the content in <body>."
  :type 'string
  :safe 'stringp
  :group 'peut-publier)

(defcustom peut-publier-default-renderer nil
  "Default renderer function.

A renderer is a function which accepts a file, returns an HTML
string, and may accept args."
  :type 'function
  :safe 'functionp
  :group 'peut-publier)

(defcustom peut-publier-default-template-type nil
  "Default page template type.

Should be a symbol.  See `peut-publier-template-alist'."
  :type 'symbol
  :safe 'symbolp
  :group 'peut-publier)

(defcustom peut-publier-template-alist nil
  "Alist of publish templates.

Key is page type (symbol) and value the associated template
function.  A template function takes a page source path and
returns an HTML string."
  :type 'alist
  :safe 'listp
  :group 'peut-publier)

(defcustom peut-publier-meta-data-maker-alist nil
  "Alist of meta-data header insert functions.

Key is a lightweight markup language extension.  Value is a
function which takes arbitrary arguments and returns a string to
be used as meta data."
  :type 'alist
  :safe 'listp
  :group 'peut-publier)


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
keys (rather than using symbols with spaces).  This function is
for use with `peut-publier-get-meta-data-alist'"
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
        "   <li>"
        "<p class=\"post-title\">"
        "<a href=\""
        (url-hexify-string (peut-publier-relative-to peut-publier-tld page-path ".html"))
        "\">"
        (peut-publier-alist-get "TITLE" (peut-publier-get-meta-data-alist page-path))
        "</a></p></li>\n"))
     (peut-publier-dir-list dir (concat "\\." peut-publier-lml "$") peut-publier--index-exclude)
     "")))

(defun peut-publier-convert-template-type (type &optional kind default)
  "Convert template TYPE to KIND.

KIND is either 'string or 'symbol.  Use 'string when writing
meta-data.  Use 'symbol when reading meta-data.

The DEFAULT symbol is the `peut-publier-default-template-type'.

See `peut-publier-template-alist' for available template types."
  (let* ((kind (or kind 'symbol))
         (default (or default
                      (if (eql kind 'symbol)
                          peut-publier-default-template-type
                        (symbol-name peut-publier-default-template-type)))))
         (cond ((eql kind 'symbol)
                (cond ((and (stringp type) (not (string-empty-p type))) (intern type))  ; convert strings to symbols
                      ((and (symbolp type) type) type)                                  ; keep any symbol not nil
                      (t  default)))
               ((eql kind 'string)
               (cond ((and (stringp type) (not (string-empty-p type))) type)  ; keep types given as strings
                     ((and (symbolp type) type) (symbol-name type))           ; non-nil symbol
                     (t default))))))

(defun peut-publier-read-date (prompt)
  "Return user-supplied date after PROMPT, defaults to today."
   (let ((org-read-date-prefer-future nil))
     (org-read-date nil nil nil prompt)))


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

Keys are given as strings.  Best used with
`peut-publier-alist-get'.

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

(defun peut-publier-make-org-meta-data (title &optional date type)
  "Return meta-data string for use in Org style source files.

Only the page TITLE is required.  The current DATE is used by
default.  TYPE is the page template type given as a string or
symbol.  Default is \"post\".  See
`peut-publier-template-alist'."
  (let ((date (or date (format-time-string "%Y-%m-%d")))
        (type (peut-publier-convert-template-type type 'string)))
    (concat
     "#+TITLE: " title "\n"
     "#+DATE: " date "\n"
     "#+TYPE: " type "\n"
     "\n")))


;; Publish:

(defun peut-publier-assemble-page (page-path)
  "Assemble PAGE-PATH into final HTML string."
  (let* ((meta-data (peut-publier-get-meta-data-alist page-path))
         (mtype (peut-publier-alist-get "TYPE" meta-data))
         (type (peut-publier-convert-template-type mtype 'symbol))
         (template (peut-publier-alist-get type peut-publier-template-alist))
         (page-content (funcall template page-path)))
    (when page-content
      (with-temp-buffer
        (insert (concat
                 "<!DOCTYPE html>\n"
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
  (interactive
   (let ((page-path (read-file-name
                     "Page file: "
                     peut-publier-src-directory
                     nil
                     t
                     (file-name-nondirectory (buffer-file-name))))
         (out-dir (read-directory-name
                   "Publish dir: "
                   (expand-file-name peut-publier-publish-directory)
                   peut-publier-publish-directory)))
     (list page-path nil)))
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


;;; Convenience:

(defun peut-publier-normalize-file-name (name &optional dir extension)
  "Return a normalized file NAME.

A normalized name begins with the current date and is file system
and URL friendly.  It is an absolute path relative to DIR or
`peut-publier-src-directory' with EXTENSION or
`peut-publier-lml'."
  (let* ((dir (or dir peut-publier-src-directory))
         (default-directory dir)  ; b/c expand-file-name
         (extension (concat "." (or extension peut-publier-lml)))
         (normalized
          (concat (format-time-string "%Y-%m-%d")
                  "-"
                  (downcase name)
                  extension)))
    (convert-standard-filename
     (expand-file-name (peut-publier-relative-to dir normalized extension) dir))))

(defun peut-publier-new-page (title &optional dir date type ext)
    "Create a new page with TITLE.

New page is automatically named using TITLE in a way that should
be friendly with the file system and web.  When used
interactively, the user is prompted for meta-data and the new
page is opened as an unsaved buffer.

DATE is a string.  It is prefixed to the file name and added to
the page meta-data.  Default is the current date formatted as
%Y-%m-%d.

TYPE corresponds to a page template found in
`peut-publier-template-alist', given as a string.  It is added
to the page meta-data.  Default is \"post\".

DIR is the save directory.  Defaults to
`peut-publier-src-directory'.

EXT is the file extension.  Default is `peut-publier-lml'."
    (interactive
     (let* ((title (read-from-minibuffer "Page title: "))
            (date (peut-publier-read-date "Date: "))
            (type (completing-read "Template type: "
                                   peut-publier-template-alist
                                   nil
                                   nil
                                   (peut-publier-convert-template-type
                                    peut-publier-default-template-type 'string))))
       (list title nil date type)))
    (let* ((dir (or dir peut-publier-src-directory))
           (date (or date (format-time-string "%Y-%m-%d")))
           (type (peut-publier-convert-template-type type 'string))
           (ext (or ext peut-publier-lml))
           (name (peut-publier-normalize-file-name title dir ext))
           (meta-data-fn (alist-get (peut-publier-convert-template-type peut-publier-lml 'symbol) peut-publier-meta-data-maker-alist))
           (meta-data (funcall meta-data-fn title date type))
           (interactivep (called-interactively-p 'any)))
      (cond ((eq interactivep t)
             (find-file
              (read-string "Save as: " name))
             (insert meta-data)
             (forward-line (point-max)))
            (t (with-temp-file name
                 (insert meta-data))
               (message "Created new page \"%s\": " name)))))

(defun peut-publier-create-site (dir)
  "Create a new site in DIR.

When called interactively, the user is prompted for the root
directory of the new site.  If the directory currently exists,
the user is alerted and may abort, leaving the file system
unchanged.  All files in DIR are removed otherwise.  If
`delete-by-moving-to-trash' is non-nil, the old site is trashed
instead of deleted.  The user is then prompted to create a new
page according to `peut-publier-default-template-type'.  To skip
the creation of a new page, simply cancel with \\[keyboard-quit].

When called from code, DIR is trashed/deleted without prompt and
a new page is not created."
  (interactive
   (let* ((dir (read-directory-name "New site: " "~/" nil nil "site/"))
          (delete-p (when (file-directory-p dir)
                      (y-or-n-p (format "Site already exists! DELETE everything in: %s?" dir)))))
   (catch 'stop-creating
     (when (eq delete-p nil)
       (throw 'stop-creating (user-error "Aborted by user.  New site not created")))
     (list dir))))
  (let* ((dir (or dir "~/site/"))
         (existed-p (file-directory-p dir))
         (src (concat dir "src/"))
         (publish (concat dir "publish/"))
         (publish-static (concat publish "static/"))
         (meta-data-fn (alist-get (peut-publier-convert-template-type peut-publier-lml 'symbol) peut-publier-meta-data-maker-alist))
         (index-file (concat src "index." peut-publier-lml))
         (about-file (concat src "about." peut-publier-lml))
         (index-meta-data (funcall meta-data-fn "Index" nil 'index))
         (about-meta-data (funcall meta-data-fn "About" nil 'about))
         (delete-by-moving-to-trash t)
         (lib-path (file-name-directory (cdr (find-function-library 'peut-publier-create-site))))
         (static-resource (concat lib-path "static/")))

    ;; runs even when dir does not exist; always returns nil
    (delete-directory dir t t)

    (when (and existed-p (not (file-directory-p dir)))
      (message "Removed %s" dir))

    ;;; create structure

    ;; create ../publish/static
    (make-directory publish-static t)
    (message "Created %s" publish-static)
    (copy-directory static-resource publish-static nil t t)
    (message "Populated %s" publish-static)

    (make-directory src t)
    (message "Created %s" src)

    (with-temp-file index-file (insert index-meta-data))
    (message "Created %s" index-file)
    (with-temp-file about-file (insert about-meta-data))
    (message "Created %s" about-file)

    (when (called-interactively-p 'any)

      ;; set globals so that user can publish immediately
      (setq peut-publier-root-directory dir)
      (message "Set `peut-publier-root-directory' to %s" dir)
      (setq peut-publier-src-directory src)
      (message "Set `peut-publier-src-directory' to %s" src)
      (setq peut-publier-publish-directory publish)
      (message "Set `peut-publier-publish-directory' to %s" publish)

      ;; create new page
      (call-interactively 'peut-publier-new-page))))


;;; Defaults:

(setq peut-publier-lml "org")

(setq peut-publier-root-directory "~/site/")
(setq peut-publier-src-directory "~/site/src/")
(setq peut-publier-publish-directory "~/site/publish/")
(setq peut-publier-author "Excalamus")
(setq peut-publier-site-name "peut-pubier")
(setq peut-publier-tld ".")
(setq peut-publier-about-img "static/about.png")
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
          "      <link rel='shortcut icon' type=\"image/x-icon\" href=\"static/favicon.ico\" />\n"))

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

(setq peut-publier-default-template-type 'post)

(setq peut-publier--default-org-backend
  (org-export-create-backend
   :name 'peut-publier
   :parent 'html
   :filters '((:filter-final-output . peut-publier--org-id-filter))))

(setq peut-publier-template-alist
  '((post . peut-publier-post-template)
    (about . peut-publier-about-template)
    (index . peut-publier-index-template)))

(setq peut-publier--index-exclude '("index" "about"))

(setq peut-publier-meta-data-maker-alist
      '((org . peut-publier-make-org-meta-data)))

(provide 'peut-publier)

;;; peut-publier.el ends here
