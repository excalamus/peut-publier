;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, blog, publish
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:

(require 'org)

;; needed for peut-publier-renderer-org-export
(require 'ox) ;; org-html-htmlize-output-type
(require 'cl-macs)  ;; cl-defun


(cl-defun peut-publier-renderer-org-export (file &optional toc section-num (output-type 'css) (backend 'html))
  "Convert FILE to html string.

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
;; toc and sec-num can be nil, output-type needs default and can be
;; nil, backend need defaults and cannot be nil. See
;; https://emacs.stackexchange.com/questions/55684/ for design
;; choices.
  (let* ((org-export-with-toc toc)
         (org-export-with-section-numbers section-num)
         (org-html-htmlize-output-type output-type)
         (backend (or backend 'html)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (org-export-as backend nil nil t nil))))

;; (defun peut-publier-renderer-org-export (file &rest args)
;;   "Export FILE to html string using `org-export-as'.

;; This function wraps `org-export-as'.  See that function for more
;; details about the arguments.

;; TOC and SECTION-NUM generate table of contents and section
;; numbers, respectively.  Defaults for each are nil.

;; OUTPUT-TYPE is 'css, 'inline-css, or nil as defined by
;; `org-html-htmlize-output-type'.  Default is 'css.

;; BACKEND is the export backend.  Default is 'html.

;; \(fn FILE &optional TOC SECTION-NUM OUTPUT-TYPE BACKEND)"
;;   (let* ((nargs (length args))
;;          (org-export-with-toc (nth 0 args))
;;          (org-export-with-section-numbers (nth 1 args))
;;          (org-html-htmlize-output-type
;;           (if (< nargs 3)
;;               'css
;;             (nth 2 args)))
;;          (backend (or (nth 3 args) 'html)))
;;     (with-temp-buffer
;;       (insert-file-contents-literally file)
;;       (org-export-as backend nil nil t nil))))

;; (defun peut-publier-renderer-org-export (file &optional toc section-num output-type backend)
;;   "Export FILE to html string using `org-export-as'.

;; This function wraps `org-export-as'.  See that function for more
;; details about the arguments.

;; TOC and SECTION-NUM generate table of contents and section
;; numbers, respectively.  Defaults for each are nil.

;; OUTPUT-TYPE is 'css, 'inline-css, or nil as defined by
;; `org-html-htmlize-output-type'.  Default is 'css.

;; BACKEND is the export backend.  Default is 'html."
;;   (let* ((org-export-with-toc toc)
;;       (org-export-with-section-numbers section-num)
;;       ;; To toggle nil, user must specify 'plain-text
;;       (org-html-htmlize-output-type
;;        (cond ((eq output-type 'css) output-type)
;;              ((eq output-type 'inline-css) output-type)
;;              ((eq output-type 'plain-text) nil)
;;              ('css)))
;;       (backend (or backend 'html)))
;;     (with-temp-buffer
;;       (insert-file-contents-literally file)
;;       (org-export-as backend nil nil t nil))))

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
