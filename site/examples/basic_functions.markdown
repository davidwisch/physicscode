---
layout: default
title: Writing Basic Functions
---

# Writing Basic Functions

Functions are essential building blocks in most programs.  They allow the developer to organize their code into smaller more logical units.  Functions help you write your programs in a way that reduces identical code from being copy/pasted all over.

Despite how essential user-defined functions *should* be to any program, many beginning programmers neglect to use them because in many cases their functionality can be replicated through simple code repitition.  Here's something to keep in mind when determining if you should be using functions or not: if you need to perform a specific action from multiple parts of your program, you should be considering the use of functions.

All major languages allow for some form of user-defined functions (although they might be called something else).  The basic declaration syntax for the four languages in this guide share many similarities with each other.

**Choose your language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Functions in Python are exceptionally easy to declare and it's possible for a function to be expressed in only one line of code (although you should avoid doing this for style reasons)!

In Python, functions are declared using the **def** keyword followed by the function name, then a list of arguments.  Functions can, but do not have to return values, and unlike in many languages can have multiple return types and can return more than one value at once.

Let's look at a very simple function definition:

File: src/python/basic_functions.py
{% highlight python %}
{% file python/basic_functions1.py %}
{% endhighlight %}

As you can see, it only took 3 lines of code to define and use our function.  Let's example a \[very\] slightly more complicated function that we can actually pass information to.  The following function lets you pass it two numbers, it then adds those numbers together and prints the result:

File: src/python/basic_functions2.py
{% highlight python %}
{% file python/basic_functions2.py %}
{% endhighlight %}

As you can see in the example, two numbers are passed to the function *add_nums* and the sum is printed on the screen.  This function is a little more usefull to us, but it would be nice if we could somehow use the output of the function for more than just display; this is where the **return** keyword comes in handy.

The **return** keyword allows the function to pass *back* information that you can retrieve for use.  Let's take a look at an example of this.

The following program has the same output as the 2nd example but retrieves the sum through the return value of *add_nums()*:

File: src/python/basic_functions3.py
{% highlight python %}
{% file python/basic_functions3.py %}
{% endhighlight %}

Now that we know how to define a function, pass data to the function, and retrieve the results, let's look a a function that might actually be usefull.

The following function calculates the [standard deviation](http://en.wikipedia.org/wiki/Standard_deviation) of a list of numbers. **NOTE** that there are different definitions for standard deviation, this is an example of the most common and includes [Bessel's Correction](http://en.wikipedia.org/wiki/Bessel's_correction).

{% highlight python %}
{% file python/basic_functions4.py %}
{% endhighlight %}

Clearly this type of function could be very useful to us in a real program.  You can verify the output of the program through Wolfram Alpha [here](http://www.wolframalpha.com/input/?i=standard+deviation+of+{25,+35,+10,+17,+29,+14,+21,+31}).  

**NOTE:** There were two times in the above example where we ensured that our calculations were done as floats instead of integers.  There's an explicit *float()* cast when calculating the average, then we multiply by 1.0 then we multiply scale our inner sum.  Without these, as long as we pass a list of integers, the result will always be *0.0* (the result, *0.0* is a float because *math.sqrt()* always returns a float).

**NOTE:** Some Python functions used in the example that might be helpful to examine further are: [sum()](http://docs.python.org/library/functions.html#sum), [len()](http://docs.python.org/library/functions.html#len), [float()](http://docs.python.org/library/functions.html#float), and [math.sqrt()](http://docs.python.org/library/math.html#math.sqrt).  This example also makes use of a [shortened syntax](/examples/reference.html#python-incrementers) avialable in Python for adding and multiplying an existing value by another one.

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
