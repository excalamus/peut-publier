;; peut-publier.el --- (P)eut-publier (E)nables (U)sers (T)o... Publish -*- lexical-binding: t; -*-

;; Author: Matt Trzcinski
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords:  static, website, generator, blog, publish
;; URL: https://github.com/excalamus/peut-publier

;;; Commentary:

;;; Code:


;; (setq my-file "~/test-post.org")
;; (funcall peut-publier-default-renderer my-file)
(setq peut-publier-default-renderer
      (lambda (file)
	(let* ((org-export-with-toc nil)
	       (org-export-with-section-numbers nil)
	       (converted
		(with-temp-buffer
		  (insert-file-contents-literally file)
		  (org-export-as 'html nil nil t nil))))
	  converted)))

(defun peut-publier-render-to-html (file &optional renderer)
  "Return html string conversion of FILE, using RENDERER.

The default RENDERER is `org-export-as'.  The user may provide
their own RENDERER.  RENDERER must be a function which accepts a
file and returns a string of html."

  )

(provide 'peut-publier)

;;; peut-publier.el ends here
