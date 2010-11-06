---
layout: default
title: Looping Structures
---

# Looping Structures

**Choose a language:**

Loops are an essential component of many programs.  They allow you to traverse arrays ([next section](/examples/basic_arrays.html)), [sample functions](/examples/physics_sampling), sort data, etc.

All of the languages in this guide support the basic **for** and **while** loops (although the requisite syntax differs) and Python supports implicit loops for arrays in the form of mapping functions.  Emphasis will be placed on the traditional *for* and *while* loops but there will be some short examples of array mapping.

**Choose your language:**

* [Python](#python)
* [Fortran](#fortran)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

As mentioned, Python supports both the *for* and *while* loop structures.  The *while* syntax is virtually identical to the syntax of C++, Java, etc. However, the *for* structure requires a bit more discussion.

**Jump to:**

* [While Loops](#python-while)
* [For Loops](#python-for)
* [Mapping](#python-map)

<a name="python-while"></a>
### While Loops

Here is an example of how to construct a *while* loop in Python:

{% highlight python %}
{% file python/basic_loops_while.py %}
{% endhighlight %}

<a name="python-for"></a>
### For Loops

The *for* loop in Python doesn't follow the pattern of *for(var, condition, increment)* that many may be used to.  In Python, the *for* loop only has two arguments, a iterator, and an iterable.  Here are some examples:

The first example is of an iterator (*index*) iterating through a list of numbers (0-8).

{% highlight python %}
{% file python/basic_loops_for1.py %}
{% endhighlight %}

It's important not to think of the set containing the numbers to be exclusive to numbers.  The following is also valid:

{% highlight python %}
{% file python/basic_loops_for2.py %}
{% endhighlight %}

This example has the same output as example #1 but achieves it through a different method.  This is also the "Pythonic way" to recreate the behavior of the traditional *for* loop:

{% highlight python %}
{% file python/basic_loops_for3.py %}
{% endhighlight %}

The *range()* function takes an integer argument and returns a list (\[0..24\] in the example) of numbers from \[0, arg-1\].  The *range* function can take arguments besides a single integer, you can read about them [here](http://docs.python.org/library/functions.html#range).

<a name="python-map"></a>
### Mapping

While you can use the *for* loop in Python as you would in other languages, it is not as cheap an operation as it in in compiled languages like C++ and thus the performance of your program will be hurt (although negligibly in most cases).  I'll "hand-wave" a few examples here, but mapping techniques seem to be difficult for beginners to use in daily practice.

This example uses the [map](http://docs.python.org/library/functions.html#map) function to add 3 to every value of a list (lists are discussed more in the [next section](/examples/basic_arrays.html)).

{% highlight python %}
{% file python/basic_loops_map1.py %}
{% endhighlight %}

The following example is used in the [reading/writing files](/examples/basic_arrays.html) section but with no explanation.  What it does is iterate through a list using Python's [float](http://docs.python.org/library/functions.html#float) function to cast its values as floating point numbers:

{% highlight python %}
{% file python/basic_loops_map2.py %}
{% endhighlight %}

**NOTE:** The *len()* function returns the length, as an integer, of a list.  In the above example *lines[i]* represents a line in the lines list and is a list itself that we pass to the *float()* function through our *map()* function.


<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Fortran supports *for* and *while* loops in much the same way that languages like C++ do.  However, in Fortran, the *for* loop is called a *do* loop, the usage is otherwise quite similar.

**Jump to:**

* [While Loops](#fortran-while)
* [For Loops](#fortran-for)

<a name="fortran-while"></a>
### While Loops

*While* loops are implemented in a very straight forward fashion.  Here is an example of a Fortran *while* loop:

{% highlight fortran %}
{% file fortran/basic_loops_while.f95 %}
{% endhighlight %}

<a name="fortran-for"></a>
### For Loops

Fortran uses the same *for(var, condition, increment)* syntax that will be familiar to many except instead of *for* the syntax is *do*.  Furthermore, if the increment is going to be 1, you can omit it (if not specified, the increment defaults to 1).

Here is an example that has the same output as the previous example:

{% highlight fortran %}
{% file fortran/basic_loops_for.f95 %}
{% endhighlight %}

It is possible to construct loop-like structures using Fortran's *GOTO* statement.  While unfortunately common, this approach is nonstandard and thus will not get an example here.


