---
layout: default
title: Reading and Writing Data Files
---

# Reading and Writing Data Files

The ability to read data from and write data to text files is extremly important.  It allows you to import existing data to run analysis on it as well as write your own results out (possibly so you can graph them, or for later review).

All four languages have the ability to read/write data files but they require different levels of understanding to understand why you have to you the syntax that you do.

When reading data from a file, you will have to know the format of the data ahead of time.  Each language has its own method for ingesting data in a particular format.

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Reading and writing data files in Python is painless.  See the examples below for further explanation.

You can find more about reading and writing files in Python in their [offial documentation](http://docs.python.org/tutorial/inputoutput.html#reading-and-writing-files).

**Jump To**

* [Reading Data Files](#python-reading)
* [Writing Data Files](#python-writing)
* [Sample Data File](#python-datafile)

<a name="python-reading"></a>
### Reading Data Files

Reading data files in Python is quite easy but there are a number of ways you can do it.  For most cases, it won't matter which one you choose.  However, depending on your method, you will be required to take different steps to access your data.

Below are working and complete examples of how to read data in from a file.

There are also a number of plugins and libraries for parsing CSV and other common data formats.  Those will not be discussed here.

#### Method #1: The Whole File At Once

This method will read an entire file (including the newline characters) into a single variable.  It's the quickest (in terms of lines of code) way to read in a file but will require the most processing to use the data.

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
### Writing Data Files

Writing datafiles in Python is quite easy.  There's only one method that this tutorial is going to go over.

Here is a working example of a program that writes data to a file:

{% highlight python %}
{% file python/basic_readwrite_write.py %}
{% endhighlight %}

**NOTE:** In Python, data is not written to the output file until either the *flush()* or *close()* methods is called (or until the program ends execution).

<a name="python-datafile"></a>
### Data File

Below is the datafile used in all of the above Python examples:

{% highlight text %}
{% file python/sampledata.txt %}
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

Reading data files in Fortran is fairly easy.  However, reading data into an array requires some trickery.  Writing to a file from Fortran is especially painless.  See the examples below for further explanation.

#### Jump To:
* [Reading Data Files](#fortran-reading)
* [Writing Data Files](#fortran-writing)
* [Sample Data File](#fortran-datafile)

<a name="fortran-reading"></a>
### Reading Data Files
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Reading data files in Fortran is fairly easy.  However, reading data into an array requires some trickery.  The trickery comes from the fact that you need to know the length of the data file (number of lines) in order to define the length of the array that will contain its data.  Common methods for solving this problem include, A) prompting the user the the number of rows of data, and B) using the first row in the datafile to define the number of data-rows.  (B) is what is demonstrated in the following example.

The following example requires that you know, explicitly, the column-format of your data.  Allocatable arrays are used so that we can dynamically set the dimentions of our 'X' array.

Read more about allocatable arrays [here](http://wikis.sun.com/display/openmp/Fortran+Allocatable+Arrays).

Read more about formatted input/output [here](http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html).

Below is a program that reads a datafile into a multidimentional array:

{% highlight fortran %}
{% file fortran/basic_readwrite_read.f95 %}
{% endhighlight %}

<a name="fortran-writing"></a>
### Writing Data Files

Writing files in Fortran is very easy and requires no advanced syntax.

Below is an example of how to write a file in Fortran:

{% highlight fortran %}
{% file fortran/basic_readwrite_write.f95 %}
{% endhighlight %}

**NOTE:** In Fortran, data is not written to the file on the WRITE() command, data is stored in the buffer until A) the CLOSE() function is called, B) the FLUSH() function is called, or C) the program completes.

<a name="fortran-datafile"></a>
### Sample Datafile

The following is the datafile used in the above Fortran 'read' example.  Notice that the first row contains the number of rows of data to read.

{% highlight text %}
{% file fortran/sampledata.txt %}
{% endhighlight %}

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
