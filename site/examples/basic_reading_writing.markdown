---
layout: default
title: Reading and Writing Data Files
---

# Reading and Writing Data Files

The ability to read data from and write data to text files is extremly important.  It allows you to import existing data to run analysis on it as well as write your own results out (possibly so you can graph them, or for later review).

All four languages have the ability to read/write data files but they require different levels of understanding to understand why you have to you the syntax that you do.

When reading data from a file, you will have to know the format of the data ahead of time.  Each language has its own method for ingesting data in a particular format.

**Choose your language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python

Reading and writing data files in Python is painless.  See the examples below for further explanation.

You can find more about reading and writing files in Python in their [offial documentation](http://docs.python.org/tutorial/inputoutput.html#reading-and-writing-files).

**Jump To**
* [Reading Data Files](#python-reading)
* [Writing Data Files](#python-writing)

<a name="python-reading"></a>
### Reading

Reading data files in Python is quite easy but there are a number of ways you can do it.  For most cases, it won't matter which one you choose.  However, depending on your method, you will be required to take different steps to access your data.

Below are working and complete examples of how to read data in from a file.

There are also a number of plugins and libraries for parsing CSV and other common data formats.  Those will not be discussed here.

#### Method #1: The Whole File At Once

This method will read an entire file (including the newline characters) into a single variable.  It's the quickest (in terms of lines of code) way to read in a file but will require the most processing to use the data.

File: src/python/basic_readwrite_wholefile.py
{% highlight python %}
{% file python/basic_readwrite_wholefile.py %}
{% endhighlight %}

You could avoid the explicit casts by converting all of the strings to floats using:

{% highlight python %}
for i in range(len(lines)):
	lines[i] = map(float, lines[i])
{% endhighlight %}

#### Method #2: Read line-by-line

This method reads the file into an array with each element representing a line.  This removes the need for the newline split (as shown in the previous example) but everything else is the same.

File: src/python/basic_readwrite_linebyline.py
{% highlight python %}
{% file python/basic_readwrite_linebyline.py %}
{% endhighlight %}

As before, you could avoid the explicit casts by converting all of the strings to floats using:

{% highlight python %}
for i in range(len(lines)):
	lines[i] = map(float, lines[i])
{% endhighlight %}

#### Method #3: \[Most Efficient\] Read line-by-line

This method, like the last one reads a file into an array line-by-line.  However, this method is more efficient.  The efficiency boost only comes in handy if you are processing really large data files.

File: src/python/basic_readwrite_efficient.py
{% highlight python %}
{% file python/basic_readwrite_efficient.py %}
{% endhighlight %}

If you want to be fancy, you can cast all of the strings as floats by replacing the line:

{% highlight python %}
lines.append(line.split(","))
{% endhighlight %}

with

{% highlight python %}
lines.append(map(float, line.split(",")))
{% endhighlight %}

In which case, the casts in the print statment would no longer be required.

<a name="python-writing"></a>
###Writing

Writing datafiles in Python is quite easy.  There's only one method that this tutorial is going to go over.

Here is a working example of a program that writes data to a file:

File: src/python/basic_readwrite_write.py
{% highlight python %}
{% file python/basic_readwrite_write.py %}
{% endhighlight %}

**NOTE:** In Python, data is not written to the output file until either the *flush()* or *close()* methods is called.

<a name="cpp"></a>
## C++

*FILL ME OUT*

<a name="fortran"></a>
## Fortran

*FILL ME OUT*

<a name="mathematica"></a>
## Mathematica

*FILL ME OUT*
