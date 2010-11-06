---
layout: default
title: Working with Arrays
---

# Working with Arrays

Arrays are incredibly useful for storing generated or related data.  Arrays are easy to comprehend if you think of them as lists of related data.  Also, they can be thought of as mathematical matrices.

For example, in a simulation of a skater on ice, you may want to keep track of their position and velocity over the duration of the simulation.  Declaring many variables to store this information is silly when instead you can store everything you need in a single array.

All  of our languages support some kind of an array.  However, their specific implementations are quite different.

**Choose a language:**

* [Python](#python)
* [Fortran](#fortran)

<a name="python"></a>

## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Arrays in Python are very different from their C++ or Fortran counterparts.  Python's implementation of the array is one of the weaker aspects of the language (at least from a computational physics perspective) but can be made to work.  Usually, although Python does have an "array" data type, array-like behavior is implemented using the 'list' type instead.  Multidimensional arrays are an especially weak point in the Python language but an example of a multidimensional structure using embedded lists is provided.

One positive about Python arrays (all of the implementations) is that they're dynamic which means you can add elements to them at anytime without worrying about length restrictions.  In many other languages, without using advanced techniques, after you declare the size of an array, that size cannot be changed.

When speed or more flexibility is needed, it is *highly* recommended that you use the [NumPy](http://numpy.scipy.org/) package for Python.  It includes a "correct" implementation of arrays and multidimensional arrays (as well as many other physics goodies).  An example using NumPy is also provided.

**Jump To:**

* [Python 'list' Type](#python-list)
* [Python 'array' Type](#python-pythonarray)
* [Multidimensional Array with Lists](#python-multilist)
* [NumPy Array](#python-numpy)
* [NumPy Multidimensional Array](#python-numpymulti)
* [Looping Through Arrays](#python-looping)

<a name="python-list"></a>
### The Python 'list' Type

This method of storing 1D data works well for relatively small data sets (approx < 10000 items, depending what you're doing with the data).  You should use [NumPy](#python-numpy) for larger data sets.

Here is an example of a list:

{% highlight python %}
{% file python/basic_arrays_list.py %}
{% endhighlight %}

<a name="python-pythonarray"></a>
### The Python 'array' Type

Unlike the Python 'list' type, arrays can only only values of a uniform type (that is, all integers, all floats, etc.).  These tend to be faster to use than lists, but still not as flexible as NumPy arrays.

For more information on Python arrays, see the [official documentation](http://docs.python.org/library/array.html).

Here is an example of an array:

{% highlight python %}
{% file python/basic_arrays_array.py %}
{% endhighlight %}

<a name="python-multilist"></a>
### Multidimensional Arrays with Lists

Multidimensional arrays are useful when you're storing data that's more complex than a single list of numbers.  For instance, if you wanted to store X, and Y coordinates, multidimensional arrays what you should use.

There's no "good" way to handle a multidimensional data type structure in Python without using NumPy.  However, in a pinch, and if your dataset isn't too large, embedded lists can do the trick.

Here's an example of using embedded lists to create a multidimensional array:

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

{% highlight python %}
{% file python/basic_arrays_numpy.py %}
{% endhighlight %}

<a name="python-numpymulti"></a>
### NumPy Multidimensional Array

As explained [above](#python-multilist), multidimensional arrays can be extremely useful.  The NumPy array library makes creating and using multidimensional arrays much easier than the embedded list method.

You will have to have NumPy installed to use this method.

Here is an example of multidimensional arrays in NumPy:

{% highlight python %}
{% file python/basic_arrays_multinumpy.py %}
{% endhighlight %}

**NOTE:** If you wanted to create an empty array, you could do so with the following:

{% highlight python %}
import numpy
# For a 4x4, fills with 0s
arr2D = numpy.zeros((4,4))
{% endhighlight %}

<a name="python-looping"></a>
### Looping Through Arrays

The ability to iterate through an array is a useful one.  It allows you to quickly traverse your data for analysis, sorting, or simply just for access.  The syntax for iterating in Python is fairly standard regardless if you're looping through a Python list, array, or numpy array.  An example of looking at 2D data is also provided.

#### 1D Data

In its most basic form, looping through a list looks like this:

{% highlight python %}
{% file python/basic_arrays_looping1.py %}
{% endhighlight %}

The same output but using an integer as an iterator:

{% highlight python %}
{% file python/basic_arrays_looping2.py %}
{% endhighlight %}

**NOTE:** Looping over NumPy arrays and Python arrays uses the same syntax.

#### 2D Data

Looping through data requires that you embed for loops within each other.  Often, when dealing with 2D data, it is more convenient to loop using the 'traditional' method (using an integer as an iterator) since we have numerical indices corresponding to every value.  An example of the "Pythonic way" as well as the "traditional" way are shown:

Here is an example using a NumPy 2D array:

{% highlight python %}
{% file python/basic_arrays_looping3.py %}
{% endhighlight %}

Here is another example, looping over the same array, except using integers as the iterators:

{% highlight python %}
{% file python/basic_arrays_looping4.py %}
{% endhighlight %}

**NOTE:** Looping over Python lists and arrays uses the same syntax.

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Arrays are implemented quite painlessly in Fortran and follow all of the same rules that you might expect.

**Jump To:**

* [One-Dimentional Arrays](#fortran-1d)
* [One-Dimentional Arrays (Short Syntax)](#fortran-1dshort)
* [Multidimensional Arrays](#fortran-multi)
* [Allocatable Arrays](#fortran-allocatable)
* [Looping Through Arrays](#fortran-looping)

<a name="fortran-1d"></a>
### One-Dimensional Arrays

Here is an example of a one-dimensional array in Fortran:

{% highlight fortran %}
{% file fortran/basic_arrays_1d.f95 %}
{% endhighlight %}

<a name="fortran-1dshort"></a>
### One-Dimensional Arrays (Short Syntax)

Fortran does provide an alternative syntax for creating 1D arrays.  Here is an example of it:

{% highlight fortran %}
{% file fortran/basic_arrays_1dshort.f95 %}
{% endhighlight %}

<a name="fortran-multi"></a>
### Multidimensional Arrays

In Fortran, multidimensional arrays function virtually identically to one-dimensional ones.  They follow the same rules, and similar declaration syntax.

Here is an example of a dimensional array in Fortran:

{% highlight fortran %}
{% file fortran/basic_arrays_multi.f95 %}
{% endhighlight %}

<a name="fortran-allocatable"></a>
### Allocatable Arrays

While Fortran arrays aren't dynamic, they do offer a hint of that convenience in the form of *allocatable arrays*.  With an allocatable array you can declare an array without specifying its size until later in the code.  You can only specify the size once (you cannot re-allocate an array), but this makes some tasks much easier.

Here is an example of an allocatable array in Fortran:

{% highlight fortran %}
{% file fortran/basic_arrays_allocatable.f95 %}
{% endhighlight %}

<a name="fortran-looping"></a>
### Looping Through Arrays

The ability to iterate through an array is a useful one.  It allows you to quickly traverse your data for analysis, sorting, or simply just for access.  The syntax for looping through data in Fortran is straightforward and concise. Examples for looping through 1D and 2D data are provided.

#### 1D Data

Here is an example of looping through a 1D array:

{% highlight fortran %}
{% file fortran/basic_arrays_looping1.f95 %}
{% endhighlight %}

#### 2D Data

Looping through 2D data requires that two DO loops be embedded within each other.

Here is an example:

{% highlight fortran %}
{% file fortran/basic_arrays_looping2.f95 %}
{% endhighlight %}

