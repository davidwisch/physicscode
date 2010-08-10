---
layout: default
title: Declaring and Using Variables in your Programs
---

# Declaring  and Using Variables in your Programs

Variables are one of the most basic building blocks of any program.  They allow you to store data (numbers, text, sets of values, etc.) for use.

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Python is a dynamically typed language which means that you can create new variables simply by assigning values to them.  In other languages (such as C++ and, to some extent, Fortran), you have to explicitly declare a variable and assign it a type (integer, double precision, string, etc.).

Here's an example of a Python program using different types of variables:

{% highlight python %}
{% file python/basic_variables.py %}
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

In Fortran, you are technically not required to declare your variables ahead of time.  However, the compiler uses the name of your variable to determine its type.  This can be confusing to new programmers and is generally a bad practice.  I will not discuss Fortran's naming conventions for variable types here.

Fortran can be set to *force* you explicitly declare variables using the **IMPLICIT NONE** statement.

In Fortran, variables can be one five different types: **INTEGER**, **REAL**, **COMPLEX**, **LOGICAL**, and **CHARACTER**.  You can read more about Fortran's data types [here](http://en.wikipedia.org/wiki/Fortran_95_language_features#Intrinsic_data_types), but the basic use cases are:

* **INTEGER** - Use for whole numbers (-2, -1, 0, 1, 2, 3, ...)
* **REAL** - Use for decimal numbers (1.3, 100.451, -1.7, 0.8...)
* **COMPLEX** - Use when a variable needs to represent a complex number
* **LOGICAL** - Use when you need a variable that can only be **TRUE** or **FALSE**
* **CHARACTER** - Use then you need a variable to represent a string of text ("My dog ate my homework")

Here is an example that demonstrates the use of some common Fortran variable types:

{% highlight fortran %}
{% file fortran/basic_variables.f95 %}
{% endhighlight %}

**NOTE** the use of the **TRIM** function to remove whitespace from the CHARACTER variables.  In Fortran, if you assign fewer characters to a variable than its maximum length, there will be whitespace at the end of the string.

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
