;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, blog, publish
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:

(require 'org)

;; (setq my-file "~/test-post.org")
;; (setq my-file "/data/data/com.termux/files/home/site/post/2020-01-20-everything-in-org.org")
;; (funcall peut-publier-default-renderer my-file)

(defun peut-publier-renderer-org-export (file &optional toc section-num backend)
  "Export FILE to html string using `org-export-as'.

TOC and SECTION-NUM generate table of contents and section
numbers, respectively.  Defaults for each are nil.  BACKEND is
the export backend to use.  Default is 'html."
  (let* ((org-export-with-toc toc)
         (org-export-with-section-numbers section-num)
         (backend (or backend 'html))
         (converted
          (with-temp-buffer
            (insert-file-contents-literally file)
            (org-export-as backend nil nil t nil))))
    converted))

(defvar peut-publier-default-renderer 'peut-publier-renderer-org-export
  "Default renderer to be used with `peut-publier-render-to-html'.")

;; (setq peut-publier-default-renderer 'peut-publier-renderer-org-export)


(defun peut-publier-render-to-html (file &optional renderer)
  "Return html string conversion of FILE using RENDERER.

The default RENDERER is `peut-publier-default-renderer'.  The
user may provide their own RENDERER.  RENDERER must be a function
which accepts a file and returns a string of html."
  (let ((renderer (or renderer peut-publier-default-renderer)))
    (funcall renderer file)))



(provide 'peut-publier)

;;; peut-publier.el ends here
