---
layout: default
title: Basic Mathematical Operations
---

# Basic Mathematical Operations

Most programming languages have the native ability to perform basic, and sometimes even moderately advanced mathematical operations.  This guide will demonstrate the use of basic arithmetic, use of functions like sine, cosine, and tangent, as well as some basic logarithms.

Mathematica has an ENORMOUS number of build in numerical functions and libraries, far exceeding the number in native Python, C++, or Fortran.

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Python sometimes gives you access to functions without you needing to do anything.  However, in the case of mathematical functions, they need to be imported before you have access to them.  The Python library that contains mathematical functions is simply called 'math'.

You can find a complete list of math functions in Python [here](http://docs.python.org/library/math.html).

Here is an example of a Python program that demonstrates the use of arithmetic, powers, logs, trigonometric functions, as well as string concatenation:

{% highlight python %}
{% file python/basic_mathematical_operations.py %}
{% endhighlight %}

**NOTE:** You can use a slightly different import statement that allows you to refer to math functions as *sin()* instead of *math.sin()* and *cos()* instead of *math.cos()*, etc.  However, doing this will pollute your namespace and could cause confusion if your program imports many things.

Here is the modified import statement:

{% highlight python %}
# 'math.' prefix no longer required
from math import *
{% endhighlight %}

<a name="cpp"></a>
## C++
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Fortran gives you access to its math functions without any import statements.

You can find a complete list of Fortran's intrinsic functions [here](http://docs.sun.com/source/819-3684/2_F95_Intrins.html):

Here is an example of a Fortran program that demonstrates the use of arithmetic, powers, logs, trigonometric functions, as well as string concatenation:

{% highlight fortran %}
{% file fortran/basic_mathematical_operations.f95 %}
{% endhighlight %}

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
