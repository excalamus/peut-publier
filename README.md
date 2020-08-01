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
- [Demo: Extend `peut-publier` to use Markdown](#demo-extend-peut-publier-to-use-markdown)

<!-- markdown-toc end -->

# **P**eut-publier **e**nables **u**sers **t**o...

* Build a website
* Using any [lightweight markup language](https://en.m.wikipedia.org/wiki/Lightweight_markup_language)
* With any workflow

`peut-publier` enables you to publish.

# Quickstart

`peut-publier` is a static website generator for [Emacs](https://www.gnu.org/software/emacs/).  It's
designed to be customized and extended using Emacs Lisp.  Websites are
built from [lightweight markup files](https://en.m.wikipedia.org/wiki/Lightweight_markup_language).  The default site is an [Org
mode](https://orgmode.org/) blog.

## Install

`peut-publier` is not currently in [MELPA](https://melpa.org/).

Install `peut-publier` by doing one of the following:

* click the "Code" button on Github, select "Download ZIP", and unzip;
  or
* clone `peut-publier` with [Git](http://git-scm.com):

  ```sh
  # create new `peut-publier` directory in user's home directory
  git clone https://github.com/excalamus/peut-publier.git ~/peut-publier
  ```

Next, add `peut-publier` to your Emacs [load-path](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html).  Place the
following in your `init.el` or evaluate ueing `M-x eval-expression`:

```emacs-lisp
;; add the peut-publier repository to the load-path
(add-to-list 'load-path "~/peut-publier/")
```

Finally, load the `peut-publier` package into Emacs:

```emacs-lisp
(require 'peut-publier)
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
pages in `src/` will be converted to HTML and placed in `publish/`.
Direct your web server to `publish/` and you've entered the world of
self-publishing.  Tu peut publier!

To publish a single page, use `M-x peut-publier-publish-page` and
follow the prompts.

## Customize

`peut-publier` allows you to customize your site through numerous
variables.  For a complete list with descriptions, check `M-x
customize-group peut-publier` or do it the old fashioned way with`C-h
peut-publier- <TAB>`.

The following are probably the variables you'd most likely want to
change.  These can be placed in your `init.el`:

```emacs-lisp
(setq peut-publier-author "Excalamus")
(setq peut-publier-site-name "peut-pubier")
```

## Extend

Read on for a demonstration of how you might modify `peut-publier` to
use *your* workflow (and not the other way around).  Afterward, you
should feel familiar enough with `peut-publier` as a library to extend
its language support, page types, and features.

`peut-publier` is written in, and intended to be extended with, Emacs
Lisp.  If you're not familiar with Emacs Lisp, that's okay.  People
who have programmed before can likely follow along without much issue.
If you're using Emacs, use `C-h f` to see a function's documentation
and source code; use `C-h v` for variables.

The best resources for learning Emacs Lisp are [An Introduction to
Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html) and the [GNU Emacs Lisp Reference
Manual](https://www.gnu.org/software/emacs/manual/elisp.html).  These may be bundled with your Emacs installation.  If
so, they can be viewed from within Emacs using the Info documentation
browser.  Type `C-h i d m Emacs Lisp Intro` or `C-h i d m Elisp` (`C-h
i` for the Info browser, `d` for the top directory, and `m` for the
manual).

# Demo: Extend `peut-publier` to use Markdown
