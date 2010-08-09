---
layout: default
title: Controlling The Flow of Your Programs
---

# Controlling The Flow of Your Programs

Very often is it necessary to change the execution of your program based on certain parameters or conditions.  Imagine if you were writing a program that would display if a number was negative or positive, or similarly, if a number was even or odd.  You would need to use an **IF** statement in order to direct your program through different paths of execution.

This guide will show you the syntax of **IF**, **ELSE IF**, and **ELSE** statments. A brief discussion on **AND** and **OR** statements is alsoincluded.


**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

If you want to get fancy, you can read about [ternary operations](http://en.wikipedia.org/wiki/Ternary_operation).

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

In Python, control is build using **if**, **elif**, and **else** statments.

Some *requirements* that you can impose in your conditionals include:

* **==** - equals
* **!==** - not equals
* **<, >** - greater than/less than
* **<=, >=** - greater than or equals/less than or equals

You can read *much* more about python control structures [here](http://docs.python.org/tutorial/controlflow.html).

Here is an example of a program that prints out if a number is positive, negative, or zero.

**NOTE:** The *raw_input* function is simply a way to accept user input into your program, it is unrelated to the proper function of the control statments.

{% highlight python %}
{% file python/basic_control.py %}
{% endhighlight %}

In addition to if/elif/else, Python supports **and** and **or** operations that are usefull.

Here is a weak example of how to use them:

{% highlight python %}
{% file python/basic_control_andor.py %}
{% endhighlight %}

There are other operations, not mentioned here, that could be usefull.  See the [official documentation](http://docs.python.org/tutorial/controlflow.html) for mroe information.

<a name="cpp"></a>
## C++
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Control statments in Fortran function as they do in many other major languages.  Program flow decisions are contained within **IF**/**END IF** statments.

Some *requirements* that you can impose on your conditionals include:

* **.EQ.** - equals
* **.NE.** - not equals
* **.GT., .LT.** - greater than/less than
* **.GE., .LE.** - greater than or equals/less than or equals

Here is an example of a program that prints out of a number is positive, negative, or zero:

{% highlight fortran %}
{% file fortran/basic_control.f95 %}
{% endhighlight %}

In addition to if/else if/else, Fortran supports **.AND.** and **.OR.** operations.

Here is a weak example of how to use them:

{% highlight fortran %}
{% file fortran/basic_control_andor.f95 %}
{% endhighlight %}

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
