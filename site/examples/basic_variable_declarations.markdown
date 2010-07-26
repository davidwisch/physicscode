---
layout: default
title: Declaring and Using Variables in your Programs
---

# Declaring  and Using Variables in your Programs

Variables are one of the most basic building blocks of any program.  They allow you to store data (numbers, text, sets of values, etc.) for later retrieval and processing.

**Choose your language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python <small>[(top)](#top)</small>

Python is a dynamically typed language which means that you can create new variables simply by assigning values to them.  In other languages (see below), you have to explicityly set the type of a variable (integer, double precision, string, etc.), not so in Python.

Here's an example of a Python program using different types of variables:

File: src/python/basic_variables.py
{% highlight python %}
{% file python/basic_variables.py %}
{% endhighlight %}

<a name="cpp"></a>
## C++ <small>[(top)](#top)</small>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran <small>[(top)](#top)</small>

In Fortran, you are technically not required to declare your variables ahead of time.  However, the compiler uses the name of your variable to determine its type.  This is a bad practice and I will not discuss those naming conventions here.

Fortran can be set to *force* you explicity declare variables using the **IMPLICIT NONE** statment.

In Fortran, variables can be one five different types: **INTEGER**, **REAL**, **COMPLEX**, **LOGICAL**, and **CHARACTER**.  You can read more about Fortran's data types [here](http://en.wikipedia.org/wiki/Fortran_95_language_features#Intrinsic_data_types), but the basic use cases are:

* **INTEGER** - Use for whole numbers (1, 2, 3, 4...)
* **REAL** - Use for decimal numbers (1.3, 100.451, 0.8...)
* **COMPLEX** - Use when a variable needs to represent a complex number
* **LOGICAL** - Use when you need a variable that can only be **TRUE** or **FALSE**
* **CHARACTER** - Use then you need a variable to represent a string of text ("My dog ate my homework")

Here is an example that demonstrates the use of some common Fortran types:

File: src/fortran/basic_variables.f95
{% highlight fortran %}
{% file fortran/basic_variables.f95 %}
{% endhighlight %}

**NOTE** the use of the **TRIM** function to remove whitespace from the CHARACTER variables.  Functions will be discussed in depth more later.

<a name="mathematica"></a>
## Mathematica <small>[(top)](#top)</small>

*FILL ME OUT*
