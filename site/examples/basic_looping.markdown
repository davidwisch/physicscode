---
layout: default
title: Looping Structures
---

# Looping Structres

**Choose your language:**

Loops are an essential component of any programs.  They allow you to traverse arrays ([next section](/examples/basic_arrays.html)), sample functions, sort data, etc.

All of the languages in this guide support the basic **for** and **while** loops (although the requisite syntax differs) and Python and Mathematica support implicit loops for arrays in the form of mapping functions.  Emphasis will be placed on the traditional *for* and *while* loops but there will be some short examples of array mapping.

**Choose your language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

As mentioned, Python supports both the *for* and *while* loop structures.  The *while* syntax is virtually identical to the syntax of C++, Java, etc. However, the *for* structure requires a bit more discussion.

**Junp to:**

* [While Loops](#python-while)
* [For Loops](#python-for)
* [Mapping](#python-map)

<a name="python-while"></a>
### While Loops

Here is an example of how to construct a *while* loop in Python:

File: src/python/basic_loops_while.py
{% highlight python %}
{% file python/basic_loops_while.py %}
{% endhighlight %}

<a name="python-for"></a>
### For Loops

The *for* loop in Python doesn't follow the pattern of *for(var, condition, increment)* that many may be used to.  In Python, the *for* loop only has two arguments, a interator, and an iterable.  Here are some examples:

The first example is of an iterator (*index*) iterating through a list of numbers (0-8).

File: src/python/basic_loops_for1.py
{% highlight python %}
{% file python/basic_loops_for1.py %}
{% endhighlight %}

It's important not to think of the set containing the numbers to be exclusive to numbers.  The following is also valid:

File: src/python/basic_loops_for2.py
{% highlight python %}
{% file python/basic_loops_for2.py %}
{% endhighlight %}

This example has the same output as example #1 but achieves it through a different method.  This is alto the "Pythonic way" to redreate the behavior of the traditional for-loop:

File: src/python/basic_loops_for3.py
{% highlight python %}
{% file python/basic_loops_for3.py %}
{% endhighlight %}

The *range()* function takes an integer argument and returns a list ([0..24] in the example) of numbers from [0, arg-1].  The range takes different argument options besides a single integer.  You can read about them [here](http://docs.python.org/library/functions.html#range).

<a name="python-map"></a>
### Mapping

While you can use the *for* loop in Python as you would in other languages, it is not as cheap an operation as it in in compiled languages like C++ and thus the performance of your program will be hurt (although negligibly in the most cases).  I'll "hand-wave" a few examples here, but mapping techniques seem to be difficult for beginners to use in daily practice.

This example uses the [map](http://docs.python.org/library/functions.html#map) function to add 3 to every value of a list (lists are discussed more in the [next section](/examples/basic_arrays.html)).

File: src/python/basic_loops_map1.py
{% highlight python %}
{% file python/basic_loops_map1.py %}
{% endhighlight %}

**NOTE:** Functions will be explained in more detail [later](/examples/basic_functions.html).

The following example is used in the [reading/writing files](/examples/basic_arrays.html) section but with no explanation.  What it does is iterate through a list using Python's [float](http://docs.python.org/library/functions.html#float) function to cast its values as floating point numbers:

File: src/python/basic_loops_map2.py
{% highlight python %}
{% file python/basic_loops_map2.py %}
{% endhighlight %}

**NOTE:** The *len()* function returns the length, as an integer, of a list.  In the baove example lines[i] represents a line in the lines list and is a list itself that we pass to the *float()* function through our *map()* function.

<a name="cpp"></a>
## C++
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
