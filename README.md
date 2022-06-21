Development has moved to: https://codeberg.org/excalamus/peut-publier

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [**P**eut-publier **e**nables **u**sers **t**o...](#peut-publier-enables-users-to)
- [Quickstart](#quickstart)
    - [Install](#install)
    - [Create a new web site](#create-a-new-web-site)
    - [Create a new web page](#create-a-new-web-page)
    - [Publish](#publish)
    - [Customize](#customize)
    - [Extend](#extend)

<!-- markdown-toc end -->

# **P**eut-publier **e**nables **u**sers **t**o...

* Build a website
* Using any [lightweight markup language](https://en.m.wikipedia.org/wiki/Lightweight_markup_language)
* With any workflow

`peut-publier` enables you to publish.

See [demo](https://excalamus.github.io/publish/index.html):

![img of demo page](./demo.png?raw=true)

# Quickstart

`peut-publier` is a static website generator for [Emacs](https://www.gnu.org/software/emacs/).  It's
designed to be customized and extended using Emacs Lisp.  Websites are
built from [lightweight markup files](https://en.m.wikipedia.org/wiki/Lightweight_markup_language).  The default site is an [Org
mode](https://orgmode.org/) blog.

## Install

`peut-publier` is not currently in [MELPA](https://melpa.org/).

## Install from source

Install `peut-publier` from source by doing one of the following:

* click the "Code" button on Github, select "Download ZIP", and unzip;
  or
* clone `peut-publier` with [Git](http://git-scm.com):

  ```sh
  # create new `peut-publier` directory in user's home directory
  git clone https://github.com/excalamus/peut-publier.git ~/peut-publier
  ```

Next, add `peut-publier` to your Emacs [load-path](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html).  Place the
following in your `init.el` or evaluate using `M-x eval-expression`:

```emacs-lisp
;; add the peut-publier repository to the load-path
(add-to-list 'load-path "~/peut-publier/")
```

Finally, load the `peut-publier` package into Emacs:

```emacs-lisp
(require 'peut-publier)
```

## Install using straight.el

After following the straight setup:

```emacs-lisp
(use-package peut-publier
  :straight (:repo "git@github.com:excalamus/peut-publier.git"))
```

## Create a new web site

Use `M-x peut-publier-create-site` to create a new web site.  The
default site is a blog.  It has an "Index" page with a
list of blog posts and an "About" page with a headshot.  Text may be
added to each.  The default directory structure, index page, and about
page are created automatically.  You'll be prompted for the creation
of your first post.

The default site directory looks like:

```
~/site/
├── publish
│   ├── about.html
│   ├── index.html
│   └── static
│       ├── about.png
│       ├── favicon.ico
│       └── style.css
└── src
    ├── about.org
    └── index.org
```

## Create a new web page

New pages can be created with `M-x peut-publier-new-page`.  The
default page type is a blog post.  You will be prompted for the post
title, date, type, and save name.  The new page is automatically
opened for you; just start writing.  By default, page meta-data is
separated from content by a single blank line.

```
#+TITLE: New post!
#+DATE: 2020-07-31
#+TYPE: post

Everything above this line is meta-data.  By default, meta-data is
separated from content by a single blank line.
```

## Publish

Publish your entire site with `M-x peut-publier-publish`.  All the
source pages in the `peut-publier-src-directory` will be converted to
HTML and placed in the `peut-publier-publish-directory`.  Direct your
web server to the `peut-publier-publish-directory` and you've entered
the world of self-publishing.  Tu peut publier!

To publish a single page, use `M-x peut-publier-publish-page` and
follow the prompts.

A trick for those on unix-like systems is to store page source files
in a separate directory (e.g. `draft/`) and create symlinks to files
you want published in the `peut-publier-src-directory`. The default
renderer will follow symlinks when publishing. To prevent rendering of
a page, simply remove the link.

## Customize

`peut-publier` allows you to customize your site through numerous
variables.  For a complete list with descriptions, check `M-x
customize-group peut-publier` or do it the old fashioned way with`C-v
peut-publier- <TAB>` or `C-f peut-publier- <TAB>`.

Customizations can be placed in a `config.el` at the top-level (or
anywhere you like).

A config may look like:

```emacs-lisp
(setq peut-publier-site-name "peut-publier")
(setq peut-publier-author "Excalamus")

;; don't use default ~/site for root
(setq peut-publier-root-directory "/home/excalamus/Projects/peut-publier.com/"))

;; since the root was redefined, redefine the source and publish
;; directories relative to the new root
(setq peut-publier-src-directory
      (concat peut-publier-root-directory "src/"))
(setq peut-publier-publish-directory
      (concat peut-publier-root-directory "publish/"))

;; define headshot image
(setq peut-publier-about-img "static/about.png")
(setq peut-publier-about-img-alt "Headshot")

;; hack together a menu
(setq peut-publier-body-preamble
  (concat
   "   <div id=\"preamble\" class=\"status\">\n"
   "      <nav>\n"
   "         <div class=\"flexcontainer\">\n"
   "            <div class=\"smallitem\">\n"
   "               <ul class=\"inline-list\">\n"
   "                  <li><a href=\".\" id=\"website\">" peut-publier-site-name "</a></li>\n"
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

;; get webpage title from post title
(defun my-html-page-title (page-path)
  "Create HTML title for PAGE-PATH."
  (concat
   "      <title>" (peut-publier-alist-get "TITLE" (peut-publier-get-meta-data-alist page-path)) "</title>\n"))

;; use my-html-page-title to render the head
(setq peut-publier-variable-head #'my-html-page-title)

;; define a postamble to be used on each page
(setq peut-publier-body-postamble
  (concat "   <div id=\"postamble\" class=\"status\">\n"
      "      <hr/>\n"
      "      <p>Powered by <a href=\"https://github.com/excalamus/peut-publier\">peut-publier</a></p>\n"
      "      <p>©" (format-time-string "%Y") " " peut-publier-author "</p>\n"
      "    </div>\n"))

;; don't list the index or about page in the post listing
(setq peut-publier--index-exclude '("index" "about"))

;; allow broken links when rendering
(setq org-export-with-broken-links 'mark)
```

## Extend

`peut-publier` is written in, and intended to be extended with, Emacs
Lisp.  If you're not familiar with Emacs Lisp, that's okay.  If you're
using Emacs, use `C-h f` to see a function's documentation and source
code; use `C-h v` for variables.

The best resources for learning Emacs Lisp are [An Introduction to
Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html) and the [GNU Emacs Lisp Reference
Manual](https://www.gnu.org/software/emacs/manual/elisp.html).  These may be bundled with your Emacs installation.  If
so, they can be viewed from within Emacs using the Info documentation
browser.  Type `C-h i d m Emacs Lisp Intro` or `C-h i d m Elisp` (`C-h
i` for the Info browser, `d` for the top directory, and `m` for the
manual).
