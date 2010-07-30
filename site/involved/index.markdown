---
layout: default
title: Getting Involved
---

<a href="http://github.com/davidwisch/physicscode"><img style="position: absolute; top: 0; right: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_right_orange_ff7600.png" alt="Fork me on GitHub" /></a>

# Getting Involved

*Physicscode* is a completely open source project and everyone is welcome (and encouraged!) to contribute.  We're looking for people help us expand our content, improve our code, design our site, and even help fix our spelling and grammar errors.

To help you get involved we suggust that you read the following sections in the order that they're listed:

**Jump to:**

* [Technology](#technology)
* [Installing the Software](#installing)
* [Getting the Code](#code)
* [Hacking the Code](#hacking)
* [Submitting your Changes](#submitting)

<a name="technology"></a>
## Technology
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

We've worked very hard to keep the technology behind *Physicscode* simple and straightforward.  We want people to be able to contribute quickly and with as little a learning curve as possible.  That being said, there are a few technologies that you'll need to become framiliar with.

### Need to Know

* **git** - [Git](http://git-scm.com) is a version control system that we use to keep track of all of our code.  It has a slight learning curve but is an absolutly essential tool when contributing to *Physicscode*.  Awesome tutorials can be found [here](http://help.github.com/), [here](http://www.kernel.org/pub/software/scm/git/docs/gittutorial.html), [here](http://help.github.com/forking/), and [here](http://progit.org/).

* **GitHub** -  [GitHub](http://github.com) hosts all of the code that powers *Physicscode*.  This is where all of the contributers submit their code, discuss new features, manage todo lists, etc.  If you're serious about contributing to *Physicscode*, you should create a GitHub account and become framilar with git and the GitHub interface.

* **Jekyll** - [Jekyll](http://jekyllrb.com) is a static site generator.  We don't directly write *Physicscode* in HTML.  Instead we write our content in Markdown (see next bullet) and let Jekyll build the HTML for us.  Jekyll lets us write our content simply, efficiently use templates to control look & feel, and perform pesudo-dynamic operations without requring us actually write the site in a dynamic language.

* **Markdown** - [Markdown](http://daringfireball.net/projects/markdow) is a text-to-HTML conversion tool.  We write all of our [non source code] content in Markdown then let Jekyll transform the Markdown into standard HTML.  Markdown is exceptionally simple to learn and makes writing content for simple websites as easy as writing any word document (maybe even easier!).

### Nice to Know

While the following technologies are used in parts of *Physicscode* they are not, strictly speaking, *required knowledge*.  However, to be an *elite* *Physicscode* developer you should probably know them, at least to some degree.

* **HTML** - [HTML]() is the markup language behind most of the internet.  Even though we [mostly] don't mix HTML and our [Markdown](http://daringfireball.net/projects/markdown/) code, HTML is still used to write our templates an provide some additional functionality that isn't supported within Markdown.  Besides, if you're going to be writing websites of any kind, you should know HTML.

* **Ruby** - [Ruby](http://www.ruby-lang.org) is a dynamic programming language that's similar to Python.  While Ruby is not a language that we use when writing our code examples, it is the language that powers certain parts of *Physicscode*.  All of our [Jekyll](http://jekyllrb.com) [plugins](#technology-plugins) are written in Ruby, part of our *deploy process* is written in Ruby, and infact even Jekyll itself is written in Ruby (although not by us).

* **Javascript/jQuery** - [jQuery](http://jquery.com/) while used as little as possible (not because we don't like it but because we want to keep the site simpe) still provides some additional dynamic functionality in various places throughout *Physicscode*.

<a name="installing"></a>
## Installing the Software
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

The bare minimum that you'll need to install before building *Physicscode* is: [Git](http://git-scm.com), [Ruby](http://www.ruby-lang.org), [Jekyll](http://jekyllrb.com), [Python](http://python.org), and the [Pygments](http://pygments.org/) package for Python.  Also, to be completely compatable, you'll need to install the rdiscount gem for ruby (see below).

**Jump to:**

* [Windows](#installing-windows)
* [Mac OSX](#installing-osx)
* [Ubuntu Linux](#installing-ubuntu)

<a name="installing-windows"></a>
### Installing on Windows

Windows is the hardest platform to install the necessary dependencies on but with a little effort, it can be done.

**Jump to:**

* [Git on Windows](#installing-windows-git)
* [Ruby on Windows](#installing-windows-ruby)
* [Jekyll on Windows](#installing-windows-jekyll)
* [Python on Windows](#installing-windows-python)
* [Pygments on Windows](#installing-windows-pygments)
* [rdiscount gem on Windows](#installing-windows-rdiscount)

<a name="installing-windows-git"></a>
#### Git on Windows

There is no official [git](http://git-scm.com) binary for Windows.  Instead you should install the windows port [msysgit](http://code.google.com/p/msysgit/).  You can download an installer from their site.

<a name="installing-windows-ruby"></a>
#### Ruby on Windows

You can download a Windows installer for Ruby from their [website](http://ruby-lang.org).  The version of Ruby used on the *Physicscode.org* server is **1.8.6**.

If you're asked, also install **rubygems**.

<a name="installing-windows-jekyll"></a>
#### Jekyll on Windows

Once you have Ruby and rubygems installed you can install Jekyll through its ruby gem using the command:

{% highlight bash %}
gem install jekyll
{% endhighlight %}

<a name="installing-windows-python"></a>
#### Python on Windows

You can download a Windows installer for Python from their [website](http://python.org).

<a name="installing-windows-pygments"></a>
#### Pygments on Windows

Please refer to [this guide](http://skim.la/2010/02/14/how-to-run-jekyll-pygmentize-on-windows/) for installing Pygments on Windows.

<a name="installing-windows-rdiscount"></a>
#### rdiscount gem on Windows

*rdiscount* can be installed the same way that Jekyll was.  Use the following command to install:

{% highlight bash %}
gem install rdiscount
{% endhighlight %}

<a name="installing-osx"></a>
### Installing on Mac OSX

The dependencies for *Physicscode* install very easily on OSX.  Many of them are already installed.

**Jump to:**

* [Git on OSX](#installing-osx-git)
* [Ruby on OSX](#installing-osx-ruby)
* [Jekyll on OSX](#installing-osx-jekyll)
* [Python on OSX](#installing-osx-python)
* [Pygments on OSX](#installing-osx-pygments)
* [rdiscount gem on OSX](#installing-osx-rdiscount)

<a name="installing-osx-git"></a>
#### Git on OSX

Please refer to [this guide](http://help.github.com/mac-git-installation/) for installing Git on OSX.

<a name="installing-osx-ruby></a>
#### Ruby on OSX

You're in luck!  Ruby is already installed on your system.

<a name="installing-osx-jekyll"></a>
#### Jekyll on OSX

Install the Jekyll gem using the following command:

{% highlight bash %}
gem install jekyll
{% endhighlight %}

<a name="installing-osx-python"></a>
#### Python on OSX

You're in luck!  Python is already installed on your system.

<a name="installing-osx-pygments"></a>
#### Pygments on OSX

##### If you have OSX Leapord or higher:

You have a utility called *easy_install* already installed and can simply run the following command to install Pygments:

{% highlight bash %}
sudo easy_install Pygments
{% endhighlight %}

##### If you are running OSX Tiger:

You should install Pygments via MacPorts using the following command:

{% highlight bash %}
sudo port install python25 py25-pygments
{% endhighlight %}

<a name="installing-osx-rdiscount"></a>
#### rdiscount gem on OSX

The rdiscount gem can be installed in the same way that Jekyll was using the following command:

{% highlight bash %}
gem install rdiscount
{% endhighlight %}

<a name="installing-ubuntu"></a>
### Installing on Ubuntu Linux

<a name="code"></a>
## Getting the Code
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

<a name="hacking"></a>
## Hacking the Code
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

<a name="submitting"></a>
## Submitting your Changes
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>
