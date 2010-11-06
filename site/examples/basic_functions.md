---
layout: default
title: Writing Basic Functions
---

# Writing Basic Functions

Functions are essential building blocks in most programs.  They allow the developer to organize their code into smaller more logical units.  Functions help you write your programs in a way that reduces identical code from being copied & pasted all over.

Despite how essential user-defined functions *should* be to any program, many beginning programmers neglect to use them because in many cases their functionality can be replicated through simple code repetition.  Here's something to keep in mind when determining if you should be using functions or not: if you need to perform a specific action from multiple parts of your program, you should be considering the use of functions.

All major languages allow for some form of user-defined functions.  The basic declaration syntax for the four languages in this guide share many similarities with each other.

**Choose a language:**

* [Python](#python)
* [Fortran](#fortran)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Functions in Python are exceptionally easy to declare and it's possible for a function to be expressed in only one line of code (although you should avoid doing this for style reasons)!

In Python, functions are declared using the *def* keyword followed by the function name, then a list of arguments.  Functions can, but do not have to return values, and unlike in many languages can have multiple return types and can return more than one value at once.

Let's look at a very simple function definition:

{% highlight python %}
{% file python/basic_functions1.py %}
{% endhighlight %}

As you can see, it only took 3 lines of code to define and use the function.  Let's example a *slightly* more complicated function that we can actually pass information to.  The following function lets you pass it two numbers, it then adds those numbers together and prints the result:

{% highlight python %}
{% file python/basic_functions2.py %}
{% endhighlight %}

As you can see in the example, two numbers are passed to the function *add_nums* and the sum is printed on the screen.  This function is a little more useful to us, but it would be nice if we could somehow use the output of the function for more than just display; this is where the *return* keyword comes in handy.  The return* keyword allows the function to pass back information for later use.

The following program has the same output as the 2nd example but retrieves the sum through the return value of *add_nums()*:

{% highlight python %}
{% file python/basic_functions3.py %}
{% endhighlight %}

Now that we know how to define a function, pass data to the function, and retrieve the results, let's look a function that might actually be useful.

The following function calculates the [standard deviation](http://en.wikipedia.org/wiki/Standard_deviation) of a list of numbers. **NOTE** that there are different definitions for standard deviation, this is an example of the most common definition including [Bessel's Correction](http://en.wikipedia.org/wiki/Bessel's_correction).

{% highlight python %}
{% file python/basic_functions4.py %}
{% endhighlight %}

Clearly this type of function could be very useful to us in a real program.  You can verify the output of the program through Wolfram Alpha [here](http://www.wolframalpha.com/input/?i=standard+deviation+of+{25,+35,+10,+17,+29,+14,+21,+31}).

**NOTE:** There were two times in the above example where we ensured that our calculations were done as floats instead of integers.  There's an explicit *float()* cast when calculating the average, then we multiply by 1.0 when we multiply scale our inner sum.  Without these, as long as we pass a list of integers, the result will always be *0.0* (the result, *0.0* is a float because *math.sqrt()* always returns a float).

**NOTE:** Some Python functions used in the example that might be helpful to examine further are: [sum()](http://docs.python.org/library/functions.html#sum), [len()](http://docs.python.org/library/functions.html#len), [float()](http://docs.python.org/library/functions.html#float), and [math.sqrt()](http://docs.python.org/library/math.html#math.sqrt).  This example also makes use of a [shortened syntax](/examples/reference.html#python-incrementers) available in Python for adding and multiplying an existing value by another one.

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

User defined functions in Fortran work essentially the same way that they do in other strongly typed languages.  Functions have a name, a return type, they can accept parameters, and can return values.

In addition to functions, Fortran also supports subroutines.  While functions and subroutines can perform similar functions, they differ in three important ways.  These are:

1. Subroutines have no type
2. Subroutines can return multiple values or no values at all
3. You use subroutines by using the **CALL** statement, not simply by referring to its name

**Jump To:**

* [Functions](#fortran-functions)
* [Subroutines](#fortran-subroutines)

<a name="fortran-functions"></a>
### Functions

While Fortran functions are fairly straight forward and follow a familiar pattern, they do have some oddities in syntax that include:

* Functions must be declared in either a **CONTAINS** block at the end of a program, or after the program (i.e. after *END PROGRAM*) itself.
* Values are not returned via a *return* keyword, instead a variable with the same name as the function is assigned the return value.  After the function completes, this variable's value is returned.
* There is a *RETURN* keyword that is used in Fortran functions although it doesn't, itself, return a value.  It functions much how *END* does elsewhere in Fortran programs by ending the execution of a function.

The basic format of a function declaration in Fortran follows the pattern:

	[return type] FUNCTION [function name] (*args)
	......
	END FUNCTION [function name]

Here's an example of a program that prints "Hello World" through a function:

{% highlight fortran %}
{% file fortran/basic_functions1.f95 %}
{% endhighlight %}

**NOTE:** The **RETURN** line is optional in this context as the function would terminate on its own.  However, it's good practice to use the *RETURN* keyword.

**NOTE:** The function is contained within after a **CONTAINS** label.  This is necessary if you want to use functions within the **PROGRAM** block.

Here is an example of the same function and output except now the function is an external function instead of an internal one.

{% highlight fortran %}
{% file fortran/basic_functions2.f95 %}
{% endhighlight %}

**NOTE:** In the 2nd example, the **CONTAINS** label was removed and the function is now declared outside of the *PROGRAM* block.  When we made the function external we also introduced the need to declare a variable in our main program with the same name as our function.

**NOTE:** In all programs after this, all functions will be internal.

The following is an example of a program that calculates the [sample standard deviation](http://mathworld.wolfram.com/StandardDeviation.html) of an array of numbers:

{% highlight fortran %}
{% file fortran/basic_functions3.f95 %}
{% endhighlight %}

You can verify the results of this program with Wolfram Alpha by going [here](http://www.wolframalpha.com/input/?i=standard+deviation+of+{25,+35,+10,+17,+29,+14,+21,+31}).

<a name="fortran-subroutines"></a>
### Subroutines

Subroutines can perform a similar as functions in Fortran problems.  Unlike functions however, they do not have an explicit return type and can return more than one value (or none).  While subroutines can be useful, they can also cause confusion because it's not always obvious what data a subroutine is manipulating.  Unlike functions, subroutines are called by using the **CALL** statement, not simply by using the subroutine's name.

Subroutines don't have an explicit return type and don't return values in the way that functions would.  Instead, passing variables into a subroutine give the subroutine access to read and write those variables.  The developer must be aware that passing variable *a* to a subroutine could effect a change in its value.  Because of this behavior, when using subroutines, the **INTENT** statement should always be used to protect variables.

The *INTENT* statement is used during a variable declaration to control what can be done with each of the parameters passed into a subroutine.  Your parameter variables can be either *read*, *write*, or *read/write*.  Declaring these explicitly will save time in debugging and prevent subroutines from causing unexpected behavior.

Here are *INTENT*'s three options:

**INTENT(IN)** - read-only. This variable can be read but not written to.
**INTENT(OUT)** - write. This variable is initially undefined but can be written to.
**INTENT(INOUT)** - read/write.  This variable can be read from and written to.

The following is an example of a subroutine that calculates the [sample standard deviation](http://mathworld.wolfram.com/StandardDeviation.html) of an array of numbers:

{% highlight fortran %}
{% file fortran/basic_functions_subroutine.f95 %}
{% endhighlight %}

Notice how the *sd* variable is passed into the subroutine, had a value assigned, then used in the *PRINT* statement after the subroutine call.

