---
layout: default
title: Style Guide
---

# Style Guide

Because *Physicscode* is a growing project with multiple collaboraters, there needs to be some uniformity in the way that content and code is written.  This document will try and establish some basic standards that all collaboraters should adhere to.

There are two basic categories of *Physicscode*, content (the website), and code (the code examples).  Each has their own standards requirements.  See below for more details.

**Jump to:**

* [Content Standards](#content)
* [Code Standards](#code)
* [Plugins](#plugins)

<a name="content"></a>
## Content Standards
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

The content of the site means anything that isn't program source code.

**Jump to:**

* [General](#content-general)
* [Markdown](#content-markdown)
* [Titles](#content-titles)
* [Section Headers](#content-headers)
* [Images](#content-images)
* [Content Plugins](#content-plugins)
* [Sitemap](#content-sitemap)

<a name="content-general"></a>
### General

<a name="content-markdown"></a>
### Makrdown

*Physicscode* is written primairly in Markdown and we'd like to keep it that way.  In exchange for Markdown's simplicity, it takes away the ability to do any advanced formatting.  It's possible to drop down into HTML within your markdown code but that's messy, you should avaoid doing it.

The rule of thumb is this:  If you can do something in Markdown, do it.  If you can't, think really hard about if the site needs whatever you're looking to add.

We know, We know, there's HTML all over most pages, but for internal links, float breaks, etc. we guess it's okay.  Got to draw the line somewhere I guess.

**Oh!** Almost forgot.  HTML is fine in the templates (obviously).

<a name="content-titles"></a>
### Titles

The title of your page and the main header's content should always match.  That is, the text that appears in the <title></title> tag should match an H1 tag at the top of your page.

The <title></title> block is populated with the content of the 'title:' setting that's expressed in the YAML at the top of every page.

<a name="content-headers"></a>
### Section Headers

Markdown makes it easy to drop H1, H2, H3, etc. tags all over your document.  Just throw some "# # #s" and you're good to go right?  Wrong!  The title of your page should be H1 (# in markdown), its subsections should be H2 (## in markdown), it's subsections should be H3, and so on.

Sections should be used to show content heirarchy, not just for creating emphasis.

In *Physicscode*, H2 tags are underlined to create a sense of separation between major sections.

<a name="content-images"></a>
### Images

All images should be stored in the physicscode/images directory.  From there you should store them in folder named after the highest level page (in terms of directory heirarchy) that image will appear on.

All images should be in the **PNG** format.

All imahes should be imported using the *image* plugin for Jekyll (see [plugins](#plugins) section).

<a name="conrent-plugins"></a>
### Content Plugins

Usage of the content plugins is described in the [plugins](#plugins) section.

<a name="content-sitemap"></a>
### Sitemap

Whenever you add a new page make sure you add it to the sitemap.  Everything in the sitemap should be listed alphabetically.

<a name="code"></a>
## Code Standards
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Standards in sourcecode are very important, especially when an inexperienced programmer is trying to read it.  These standards are fairly loose but should be adhered to.

**Jump to:**

* [Commenting](#code-comments)
* [Naming](#code-naming)
* [Curly Braces](#code-blocks)
* [Spacing](#code-spacing)
* [Line Endings](#code-newlines)
* [Intrinsic functions](#code-functions)

<a name="code-comments"></a>
### Commenting

Every program needs to have a comment on the first line of the format:

	File: src/[language]/filename.ext

That is, if a Python program was named "array_multiplier.py*, its first line would be:

{% highlight python %}
# File: src/python/array_multiplier.py
{% endhighlight %}

A few lines down from the filename comment you should put a description of what that program does.

If you're writing any code in the "basics" section, make sure its well documented.  Anytime you write a line of code that someone may look at and think,"why did they do that?" comment it.

Extensive use of comments can make simple programs difficult to read for beginners.  Comment generously but use discretion too.

<a name="code-naming"></a>
### Naming

Some people like to name their functions and variables using the CamelCaseMethod of naming.  This is where in string of connected words (no spaces), the first letter of each word is a capitol letter.  Other people separate their words with_understores.  Either one is fine, do whatever feels most comfortable to you, just stay consistent within your own code.

Don't break commonly accepted naming conventions (*eg* don't name a variable 'INTEREST_RATE' (all capitol letters) if you intend to change the value of that variable later).

<a name="code-blocks"></a>
### Curly Braces

Where to put a leading curly brace (in languages that require them) is a point of contention for many programmers.

Some people like to put them directly after their class/function/etc. definition like this:

{% highlight javascript %}
function name(args){
	...
}
{% endhighlight %}

Other people put the first brace on the first line after the definitions like this:

{% highlight javascript %}
function name(args)
{
	...
}
{% endhighlight %}

Either one is fine, just make sure your code is self-consistent

<a name="code-spacing"></a>
### Spacing

Spacing can mean a few things.  Let's start of with tabs vs. spaces.  It doens't matter, use whatever you want.  However, please use a tab width of 4 spaces.

Onto the other meaning uses of spaces.  Put spaces in between arithmatic operators, after commas, etc.  This makes code easier to read.

Here's some examples of this:

{% highlight python %}
result = var1 + var 2 # Good

result=var1+var2 # Bad
{% endhighlight %}

{% highlight python %}
list = [item1, item2, item3] # Good

list=[item1,item2,item3] # Bad
{% endhighlight %}

**Don't** however, put extra spaces in between things *willy-nilly*, at a certain point it gets distracting.

Here's an example from order of operations problems.  When doing work with long strings with many "()" its usually better to break the operation up onto multiple lines.  However, if you do end up putting the whole string on one line, things can get ugly, even worse than how they started.  Take a look:

{% highlight python %}
num = ((2/4*(5-3)**(2/3.4+(4*2))/4**2)/4)**7.2 # Ugly no matter what
num = ( ( 2 / 4 * ( 5 - 3 ) ** ( 2 / 3.4 + ( 4 * 2 ) ) / 4 ** 2 ) / 4 ) ** 7.2 # Even worse!
{% endhighlight %}

**Yes** you will see that from time to time in code.  Somtimes spaces are good, sometimes they can make a situation completely unreadable.  Use some discresion but always make sure you're self consistent.

<a name="code-newlines"></a>
### Line Endings

The *Physicscode* server runs Ubuntu and both of the maintainers use POSIX systems to write the site.  When writing code or editing the site please make sure that you're using UNIX (\n) line endings.  Any real text editor will have an option to toggle between UNIX and Windows line endings.

<a name="code-functions"></a>
### Intrinsic Functions

Don't use any builtin function without explaining what it does (unless it's already been explained elsewhere).  This is especially true in the "basics" section of the code examples

<a name="plugins"></a>
## Plugins
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*Physicscode* has wrritten some custom plugins and behaviors to make our site function better.

### Jekyll Plugins

There are three plugins that *Physicscode* wrote to extend the default behavior of Jekyll.

#### image

The *image* plugin adds a [Liquid](http://www.liquidmarkup.org/) tag to Jekyll that automakes image insertion.  This was written A) so that less HTML would have to be embedded into the Markup documents and B) make it easier for developers to stay within standards.

The plugin assumes that **ALL** images reside somewhere in the physicscode/site/images directory and assumes they have a file extension **.png**.

To use the plugin, use a the following syntax.

&#123;% image funny/lolcat %&#125;

That line would insert the following HTML into the page.

{% highlight html %}
<img src="/images/funny/lolcat.png"/>
{% endhighlight %}

Note how it appended the *.png* exrension and prefixed the path with */images/*.

Use this plugin to insert an image into a page.

*image* plugin is located at:

	physicscode/site/_plugins/include_image.rb

#### file

The *file* plugin is used to import sourcecode files into a makrdown documents.  We wanted an ability to display people sourcecode in the browser but not require them to copy/paste code from their sample programs into the makrdown document.  The *file* plugin, at build time, reads a file, and returns its contents as a string into the markdown document.  Surround the *file* call with &#123;% highlight [language] %&#125; [code] &#123;% endhighlight %&#125; tags for syntax highlighting of the file.

This plugin asumes that all sourcecode is contained within the physicscode/src directory.  Place code in whatever subfolder matches the files language.

Here's an example of importing and highlighting one Python and one Fortran file:

&#123;% file python/python_file.py %&#125;
&#125;% file fortran/fortran_file.f95 %&#125;

*file* plugin is located at:

	physicscode/site/_plugins/include_file.rb

#### commit_hash

The *commit_hash* plugin finds and returns the hash of the current version's commit.  This is the plugin that prints out the content after "@ commit:" in the site's footer.  This is just an easy way to make sure the site is correctly updating when we push content.

*commit_hash* plugin is located at:

	physicscode/site/_plugins/include_commit.rb

### Javascript Actions

While this is not a plugin, it's a behavior worth mentioning.  When a link is generate in markdown, by default, following it will open the link in the current window/tab.  Since most external links in *Physicscode* are for reference, it makes more sense to have external links open in a new window/tab.

There's a bit of jQuery code at the top of the *default.html* template that scans all of the links on a page, checks if they have an http: prefix (because internal links don't) and if they do add the arrtibute *target="_blank"* thus causing the link to open in a new window/tab.
