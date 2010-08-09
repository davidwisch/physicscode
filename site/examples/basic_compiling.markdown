---
layout: default
title: Compiling and running your program
---

# Compiling and Running a program

*Explanation on compiling*

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Python programs are not compiled, they're *interpreted*.  This means that you can simply run the same file that you write code in.  Here's an example of Python:

{% highlight python %}
{% file python/basic_running.py %}
{% endhighlight %}

Simply placing that text in a file and running it with:

{% highlight bash %}
python basic_running.py
{% endhighlight %}

will produce the output "Hello World!"... no compiling needed..

<a name="cpp"></a>
## C++
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Fortran 90/95 programs are compiled using either the *g95* or *gfortran* commands.  Here is a program, written in Fortran, that prints "Hello World" two different ways:

{% highlight fortran %}
{% file fortran/basic_running.f95 %}
{% endhighlight %}

To compile a Fortran program, type:

{% highlight bash %}
g95 basic_running.f95

#Or, if you're using gfortran
gfortran basic_running.f95
{% endhighlight %}

Either of these commands will produce a file named **a.out** (on Windows, **a.exe**).  To run this file, simply type:

{% highlight bash %}
#In OSX
a.out

#In Linux
./a.out

#In Windows
a.exe
{% endhighlight %}

Optionally, if you don't like the name *a.out*, you can specify a different name with:

{% highlight bash %}
g95 basic_running.f95 -o basic_running

#In gfortran
gfortran basic_running.f95 -o basic_running
{% endhighlight %}

Both of the above commands will produce an executable named *basic_running* instead of *a.out*.

**NOTE:** From now on, all Fortran examples will be tested and shown using the g95 compiler.  Don't worry if you're using gfortran, the two are very similar.

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
