---
layout: default
title: Getting Involved
---

<a href="http://github.com/davidwisch/physicscode"><img style="position: absolute; top: 0; right: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_right_orange_ff7600.png" alt="Fork me on GitHub" /></a>

# Getting Involved

*Physicscode* is a completely open source project and everyone is welcome (and encouraged!) to contribute.  We're looking for people help us expand our content, improve our code, design our site, and even help fix our spelling and grammar errors.

To help you get involved we suggest that you read the following sections in the order that they're listed:

**Jump to:**

* [Technology](#technology)
* [Installing the Software](#installing)
* [Getting the Code](#code)
* [Building the Site](#building)
* [Hacking the Code](#hacking)
* [Submitting your Changes](#submitting)

**NOTE:** It is also recommended that you read the [Style Guide](/involved/style.html) before submitting any changes to make sure that your changes are compatible.

<a name="technology"></a>
## Technology
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

We've worked very hard to keep the technology behind *Physicscode* simple and straightforward.  We want people to be able to contribute quickly and with as little a learning curve as possible.  That being said, there are a few technologies that you'll need to become familiar with.

### Need to Know

* **git** - [Git](http://git-scm.com) is a version control system that we use to keep track of all of our code.  It has a slight learning curve but is an absolutely essential tool when contributing to *Physicscode*.  Awesome tutorials can be found [here](http://help.github.com/), [here](http://www.kernel.org/pub/software/scm/git/docs/gittutorial.html), [here](http://help.github.com/forking/), and [here](http://progit.org/).

* **GitHub** -  [GitHub](http://github.com) hosts all of the code that powers *Physicscode*.  This is where all of the contributers submit their code, discuss new features, manage to-do lists, etc.  If you're serious about contributing to *Physicscode*, you should create a GitHub account and become familiar with git and the GitHub interface.

* **Jekyll** - [Jekyll](http://jekyllrb.com) is a static site generator.  We don't directly write *Physicscode* in HTML.  Instead we write our content in Markdown (see next bullet) and let Jekyll build the HTML for us.  Jekyll lets us write our content simply, efficiently use templates to control look & feel, and perform pseudo-dynamic operations without requiring us actually write the site in a dynamic language.

* **Markdown** - [Markdown](http://daringfireball.net/projects/markdown) is a text-to-HTML conversion tool.  We write all of our [non source code] content in Markdown then let Jekyll transform the Markdown into standard HTML.  Markdown is exceptionally simple to learn and makes writing content for simple websites as easy as writing any word document (maybe even easier!).

### Nice to Know

While the following technologies are used in parts of *Physicscode* they are not, strictly speaking, *required knowledge*.  However, to be an *elite* *Physicscode* developer you should probably know them, at least to some degree.

* **HTML** - [HTML]() is the markup language behind most of the internet.  Even though we [mostly] don't mix HTML and our [Markdown](http://daringfireball.net/projects/markdown/) code, HTML is still used to write our templates and provide some additional functionality that isn't supported within Markdown.  Besides, if you're going to be writing websites of any kind, you should know HTML.

* **Ruby** - [Ruby](http://www.ruby-lang.org) is a dynamic programming language that's similar to Python.  While Ruby is not a language that we use when writing our code examples, it is the language that powers certain parts of *Physicscode*.  All of our [Jekyll](http://jekyllrb.com) [plugins](/involved/style.html#plugins) are written in Ruby, part of our deploy process is written in Ruby, and in fact even Jekyll itself is written in Ruby (although not by us).

* **Javascript/jQuery** - [jQuery](http://jquery.com/) while used as little as possible (not because we don't like it but because we want to keep the site simple) still provides some additional dynamic functionality in various places throughout *Physicscode*.

<a name="installing"></a>
## Installing the Software
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

The bare minimum that you'll need to install before building *Physicscode* is: [Git](http://git-scm.com), [Ruby](http://www.ruby-lang.org), [Jekyll](http://jekyllrb.com), [Python](http://python.org), and the [Pygments](http://pygments.org/) package for Python.  Also, to be completely compatible, you'll need to install the rdiscount gem for ruby (see below).

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

After git is installed, please complete the steps in the [Getting started with Git and GitHub](http://help.github.com/) guide hosted on GitHub.

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

After git is installed, please complete the steps in the [Getting started with Git and GitHub](http://help.github.com/) guide hosted on GitHub.

<a name="installing-osx-ruby"></a>
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

Ubuntu (Linux in general) an exceptionally easy platform to develop *Physicscode* on.  Installing most of the dependencies is mostly a one-line operation.

**Jump to:**

* [Git on Ubuntu](#installing-ubuntu-git)
* [Ruby on Ubuntu](#installing-ubuntu-ruby)
* [Jekyll on Ubuntu](#installing-ubuntu-jekyll)
* [Python on Ubuntu](#installing-ubuntu-python)
* [Pygments on Ubuntu](#installing-ubuntu-pygments)
* [rdiscount gem on Ubuntu](#installing-ubuntu-rdiscount)

<a name="installing-ubuntu-git"></a>
#### Git on Ubuntu

Installing git on Ubuntu is a one line command.  Install using the following command:

{% highlight bash %}
sudo apt-get install git-core
{% endhighlight %}

<a name="installing-ubuntu-ruby"></a>
#### Ruby on Ubuntu

Installing ruby on Ubuntu is a one line command.  Install using the following command:

{% highlight bash %}
sudo apt-get install ruby
{% endhighlight %}

<a name="installing-ubuntu-jekyll"></a>
#### Jekyll on Ubuntu

Installing ruby on Ubuntu is a one line command.  Install using the following command:

{% highlight bash %}
gem install jekyll
{% endhighlight %}

<a name="installing-ubuntu-python"></a>
#### Python on Ubuntu

You're in luck!  Python is already installed on your system.

<a name="installing-ubuntu-pygments"></a>
#### Pygments on Ubuntu

Installing Pygments on Ubuntu is a one line command.  Install using the following command:

{% highlight bash %}
sudo apt-get install python-pygments
{% endhighlight %}

<a name="installing-ubuntu-rdiscount"></a>
#### rdiscount gem on Ubuntu

Installing the rdiscount gem in Ubuntu is a one line command.  Install using the following command:

{% highlight bash %}
gem install rdiscount
{% endhighlight %}

<a name="code"></a>
## Getting the Code
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

##### Look but not touch
If you're interested in just looking at the code, you can download it by clicking on the "Download Source" button (see below) button on *Physicscode*'s [GitHub Page](http://github.com/davidwisch/physicscode).

{% image involved/downloadsourcebutton %}

You could also clone the repository to your computer using the following command:

{% highlight bash %}
git clone git://github.com/davidwisch/physicscode.git
{% endhighlight %}

##### Look and touch

If you want a copy of the source code so that you can edit it and contribute changes, you should fork the *Physicscode* project.  GitHub has a great guide to forking repositories [here](http://help.github.com/forking/).

<a name="building"></a>
## Building the Site

Now that you have the code downloaded and all of the dependences installed, it's time to build the site.  Change directories into the physicscode/site directory.  This is the directory that you must launch Jekyll from in order for the site to correctly build.

Once in the directory, run the following from the command line to build the site:

{% highlight bash %}
jekyll --server
{% endhighlight %}

**NOTE:** The *--server* flag hosts the site on port 4000 so to see the site you'll have to navigate to [http://localhost:4000](http://localhost:4000).  We're using the server option because opening the site from the generated HTML files (located in physicscode/_site after the build) causes internal links not to function properly.

Every time you change a file you'll have to stop the Jekyll process (*CTRL-C*) and restart it.  Alternatively, you could specify the *--auto* flag to have it automatically detect file changes and reload the server.  Here is an example:

{% highlight bash %}
jekyll --server --auto
{% endhighlight %}

Use *CTRL-C* to end this jekyll process.

**NOTE:** You should read the [Style Guide](/involved/style.html) before changing any part of the site to make sure that your changes will be allowed.

**NOTE:** *Physicscode* uses some custom plugins to extend the native capabilities of Jekyll and some Javascript to change the default behavior of links generated in Markdown.  Read about these on the [style](/involved/style.html#plugins) page.

<a name="hacking"></a>
## Hacking the Code
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Now that you have a local copy of the *Physicscode* repository, feel free to start editing whatever you'd like.  This [git tutorial](http://www.kernel.org/pub/software/scm/git/docs/gittutorial.html) and the [forking guide](http://help.github.com/forking/) on GitHub have good tips on how to use git effectively.

**NOTE:** You should read the [Style Guide](/involved/style.html) before chancing any part of the site to make sure that your changes will be allowed.

**NOTE:** *Physicscode* uses some custom plugins to extend the native capabilities of Jekyll and some Javascript to change the default behavior of links generated in Markdown.  Read about these on the [style](/involved/style.html#plugins) page.

<a name="submitting"></a>
## Submitting your Changes
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

When you've completed a change that you think is ready for *Physicscode.org* you'll need to *push* the changes to your fork in GitHub.  Then, once your changes have been pushed, click the "Pull Request" in your forked repository (see image below).  Make sure at least one maintainers is selected, describe your changes and click "Send Pull Request" (see image below).  That's it!  A maintainer will review your changes and merge them into the live site if they're ready!

{% image involved/pullrequestbutton %}

{% image involved/sendpullrequest %}
