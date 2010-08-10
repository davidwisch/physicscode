---
layout: default
title: Sampling a Function
---

# Sampling a Function

In many physics programs you may be required to "sample a function".  What this means is to find the value of the function at various values (or its independent variable(s)) and store this data for output (graphing) or some kind of further analysis.

The "Basic Operations" section of [examples](/examples) covers all of the skills needed to sample a function but since it's such a common task, we'll show a complete example here.

The following program samples the function *cos(x<sup>2</sup>)* 1000 times in the range of 0 &rarr; &Pi;, stores the data in an array, then prints the data to the screen.

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

The following example shows how to sample a function in Python, store the data in an array, and print the data to the screen.

{% highlight python %}
{% file python/physics_sampling.py %}
{% endhighlight %}

**NOTE** that in this case, defining a custom function to find the value of *cos(x<sup>2</sup>)* didn't provide us with a significant advantage over just hardcoding the function into the loop.  However, as functions become more complex and take additional parameters, custom function definitions provide an increased advantage.  It is good practice to use them.

<a name="cpp"></a>
## C++
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

The following example shows how to sample a function in Fortran, store the data in an array, and print the data to the screen.

{% highlight fortran %}
{% file fortran/physics_sampling.f95 %}
{% endhighlight %}

**NOTE** that in this case, defining a custom function to find the value of *cos(x<sup>2</sup>)* didn't provide us with a significant advantage over just hardcoding the function into the loop.  However, as functions become more complex and take additional parameters, custom function definitions provide an increased advantage.  It is good practice to use them.

<a name="mathematica"></a>
## Mathematica
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

*FILL ME OUT*
