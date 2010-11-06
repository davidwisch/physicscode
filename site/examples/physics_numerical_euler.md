---
layout: default
title: Numerical Integration- Euler Method
---

# Numerical Integration - Euler Method

The Euler method is the most basic approach to numerical integration of an equation.  The Euler method, while extremely simple,  can produces significant amounts of error over time (*O(h<sup>2</sup>*), where *h* is the step size) and is considered less stable than other methods.  The Euler method can be use on any [Ordinary Differential Equation](http://mathworld.wolfram.com/OrdinaryDifferentialEquation.html) provided an initial condition.

You can read more about the Euler method on [Wolfram Mathworld](http://mathworld.wolfram.com/EulerForwardMethod.html) and [Wikipedia](http://en.wikipedia.org/wiki/Euler_method).

There is a slight variation of the Euler method known as the **Euler-Cromer** method (sometimes called the Semi-implicit Euler method).  This method is considered more accurate than that basic Euler method.  The examples in this guide demonstrate the Euler method but conversion to the Euler-Cromer method is straightforward.  You can read more about the Euler-Cromer method on [Wikipedia](http://en.wikipedia.org/wiki/Euler-Cromer_algorithm).


In this guide, we'll solve for the position of an object that is launched from the ground at an angle.  The object will be launched from an angle of 30 degrees (from the horizontal) with an initial velocity of 112 m/s.

These are 2D simulations (that is, they track the X and Y coordinates and can account for a simple constant horizontal force (drag or push)).

**NOTE** that some of the programs could be expressed with fewer lines of code but they're written the way they are to maintain a comfortable level of abstraction.

**NOTE** that these simulations could be easily changed to use the [Euler-Cromer](http://en.wikipedia.org/wiki/Euler-Cromer_algorithm) method simply by swapping the order that position and velocity are calculated in.

**Choose a language:**

* [Python](#python)
* [Fortran](#fortran)

See the [graphs](#graphs) from this program.

<a name="python"></a>
## Python
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

{% highlight python %}
{% file python/physics_numerical_euler.py %}
{% endhighlight %}


<a name="fortran"></a>
## Fortran
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

Below is a Fortran implementation of the program.

{% highlight fortran %}
{% file fortran/physics_numerical_euler.f95 %}
{% endhighlight %}

<a name="graphs">
## Graphs
<div class="to-top"><a href="#top">(Top of Page)</a></div>
<div style="clear: both;"></div>

All of the following graphs are generated using the output from the [Python](#python) program above and made using [Gnuplot](http://www.gnuplot.info/).

This graph shows the height of the object vs. its travel time (there is no horizontal drag).

{% image examples/physics/numerical_euler_height_v_time_nodrag %}

The following graph shows the height of the object as a function of the horizontal distance traveled (there is no horizontal drag).

{% image examples/physics/numerical_euler_height_v_distance_nodrag %}

The following graph shows the height of the object as a function of the horizontal distance traveled.  However, unlike the previous graphs, there was a horizontal drag applied to the object.  As a result, you can see the object traveled less of a distance and its trajectory is no longer symmetrical.

{% image examples/physics/numerical_euler_height_v_distance_drag %}

The following graph is similar to the previous one except instead of a horizontal *drag*, there's a horizontal *push* applied.

{% image examples/physics/numerical_euler_height_v_distance_push %}
