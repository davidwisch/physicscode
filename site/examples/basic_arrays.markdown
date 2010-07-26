---
layout: default
title: Working with Arrays
---

# Working with Arrays

Arrays are incredibly usefull for storing generated or related data.  Arrays are easy to comprehend if you think of them as lists of related data.  Also, they closely resemble matricies from mathematics.

For example, in a simulation of a skater on ice, you may want to keep track of their position and velocity over the duration of the simulation.  Declaring many variables to store this information is silly when instead you can store everything you need in an array.

All four of our languages support some kind of an array.  Their specific implementations are quite different, however.

**Choose your language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>

## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Arrays in Python are very different from their C++ or Fortran counterparts.  Python's implementation of the array is one of the weaker aspects of the language (at least from a computational physics perspective) but can be made to work.  Usually, although Python does have an "array" datatype, array-like behavior is implemented using the 'list' datatype instead.  Multidimentional arrays are an especially weak point in the Python language but an example of a multidimentional structure using embedded lists is provided.

One positive about Python arrays (all of the implementations) is that they're dynamic which means you can add elements to them at anytime.  In many other languages, without using advanced techniques, after you declare the size of an array, it cannot be changed.

When speed or more flexibility is needed, it is reccomended that you use the [NumPy](http://numpy.scipy.org/) addon package for Python.  It includes a "correct" implementation of arrays and multidimentional arrays (as well as many other physics goodies).  An example using NumPy is also provided.

**Jump To:**

* [Python 'list' Type](#python-list)
* [Python 'array' Type](#python-pythonarray)
* [Multidimentional Array with Lists](#python-multilist)
* [NumPy Array](#python-numpy)
* [NumPy Multidimentional Array](#python-numpymulti)

<a name="python-list"></a>
### The Python 'list' Type

This method of storing 1D data works well for relativly small data sets (< 10000 items).  You should use [NumPy](#python-numpy) for larger data sets.

Here is an example of a list:

File: src/python/basic_arrays_list.py
{% highlight python %}
{% file python/basic_arrays_list.py %}
{% endhighlight %}

<a name="python-pythonarray"></a>
### The Python 'array' Type

Unlike the Python 'list' type, arrays can only only values of a uniform type (that is, all integers, all floats, etc.).  These tend to be faster to use than lists, but still not as flexible as NumPy arrays.

For more information on Python arrays, see the [official documentation](http://docs.python.org/library/array.html).

Here is an example of an array:

File: src/python/basic_arrays_array.py
{% highlight python %}
{% file python/basic_arrays_array.py %}
{% endhighlight %}

<a name="python-multilist"></a>
### Multidimentional Arrays with Lists

Multidimentaional arrays are usefull when you're storing data that's more complex than a single list of numbers.  For instance, if you wanted to store X,Y coordinates, multidimentional arrays lend themselves to this task quite nicely.

There's no "good" way to handle a multidimentional datatype  structure in Python without using NumPy.  However, in a pinch, and if your dataset isn't too large, embededded lists can do the trick.

Here's an example of using embedded lists to create a multitdimentional array:

File: src/python/basic_arrays_multilist.py
{% highlight python %}
{% file python/basic_arrays_multilist.py %}
{% endhighlight %}

**NOTE:** If you wanted to create an empty array using this method, you could do so by doing the following:

{% highlight python %}
# Creates a 4x4 array with 0s
list4x4 = [[0 for col in range(4)] for row in range(4)]
{% endhighlight %}

<a name="python-numpy"></a>
### NumPy Array

Using the NumPy implementation of arrays is by far the most efficient.  They're also easy to use and well documented.  To use NumPy arrays, you will have to install NumPy.  See their [installation instructions](http://new.scipy.org/download.html) for assistance.

For additional documentation on NumPy arrays, see [this page](http://docs.scipy.org/doc/numpy/user/basics.creation.html#arrays-creation).

Here is an example of how to use a arrays in NumPy:

File: src/python/basic_arrays_numpy.py
{% highlight python %}
{% file python/basic_arrays_numpy.py %}
{% endhighlight %}

<a name="python-numpymulti"></a>
### NumPy Multidimentaional Array

As explained [above](#python-multilist), multidimentional arrays can be extremly usefull.  The NumPy array library makes creating and using multidimentional arrays much easier than the embedded list method.

You will have to have NumPy installed to use this method.

Here is an example of multidimentional arrays in NumPy:

File: src/python/basic_arrays_multinumpy.py
{% highlight python %}
{% file python/basic_arrays_multinumpy.py %}
{% endhighlight %}

**NOTE:** If you wanted to create an empty array, you could do so with the following:

{% highlight python %}
import numpy
# For a 4x4, fills with 0s
arr2D = numpy.zeros((4,4))
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

Arrays are implemented quite painlessly in Fortran and follow all of the same rules that you might expect.

**Jump To:**

* [One-Dimentional Arrays](#fortran-1d)
* [One-Dimentional Arrays (Short Syntax)](#fortran-1dshort)
* [Multidimentional Arrays](#fortran-multi)
* [Allocatable Arrays](#fortran-allocatable)

<a name="fortran-1d"></a>
### One-Dimentional Arrays

Here is an example of a one-dimentional array in Fortran:

File: src/fortran/basic_arrays_1d.f95
{% highlight fortran %}
{% file fortran/basic_arrays_1d.f95 %}
{% endhighlight %}

<a name="fortran-1dshort"></a>
### One-Dimentional Arrays (Short Syntax)

Fortran does provide an alternative syntax for creating 1D arrays.  Here is an example of it:

File: src/fortran/basic_arrays_1dshort.f95
{% highlight fortran %}
{% file fortran/basic_arrays_1dshort.f95 %}
{% endhighlight %}

<a name="fortran-multi"></a>
### Multidimentional Arrays

In Fortan, multidimentional arrays function virtually identically to one-dimentional ones.  They follow the same rules, and similar declaration syntax.

Here is an example of a multidimentional array in Fortran:

File: src/fortran/basic_arrays_multi.f95
{% highlight fortran %}
{% file fortran/basic_arrays_multi.f95 %}
{% endhighlight %}

<a name="fortran-allocatable"></a>
### Allocatable Arrays

While Fortran arrays arn't dynamic, they do offer a hint of that convenience in the form of allocatable arrays.  With an allocatable array you can declare an array without specifying its size until later in the code.  You can only specify the size once (you cannot re-allocate an array), but this makes some tasks much easier.

Here is an example of an allocatable array in Fortran:

File: src/fortran/basic_arrays_allocatable.f95
{% highlight fortran %}
{% file fortran/basic_arrays_allocatable.f95 %}
{% endhighlight %}

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
