---
layout: default
title: An Interesting Function
---

# An Interesting Function

## Description

In this example program we will sample a function and write its values to a file for graphing (graphs provided).  Besides illustrating some essential programming skills, this program also demonstrates why simply writing code is often not enough to solve a problem or complete an assignment).

Too often, when asked to analyze a graph they've made, students will quickly glance at the graph looking for prominent features, draw conclusions on those and move on.  However, as is the case with this program, such a simple analysis is not enough.  It is sometimes necessary to really *think* about a graph (and the program that generated it) to make sure that macro-level features on the graph aren't obscuring something more important.

The equation that will be graphed has asymptotes that make most of the graph appear as a straight horizontal line.  Such a simple analysis is insufficient to uncover the detail that can only discovered by "zooming in" past the asymptotes.

## The Equation

{% image examples/putting_interesting_1 %}

## The Program

The above equation will be sampled 250,000 times in the range of 1 &rarr; 5.

**Choose a language:**

* [Python](#python)
* [C++](#cpp)
* [Fortran](#fortran)
* [Mathematica](#mathematica)

<a name="python"></a>
### Python
<div style="clear:both;"></div>

The following is an implementation of the program in Python.

{% highlight python %}
{% file python/putting_interesting.py %}
{% endhighlight %}

<a name="cpp"></a>
### C++
<div style="clear:both;"></div>

*FILL ME OUT*

<a name="fortran"></a>
### Fortran
<div style="clear:both;"></div>

{% highlight fortran %}
{% file fortran/putting_interesting.f95 %}
{% endhighlight %}

<a name="Mathematica"></a>
### Mathematica
<div style="clear:both;"></div>

*FILL ME OUT*

## The Graphs

All of the below graphs are generated using the output from the [above](#python) Python file and [gnuplot](http://www.gnuplot.info/).

This is the graph as it looks after simply graphing it.  It appears to be mostly a straight line, but a single outlying point and huge y-axis range indicates that there might be an asymptote.

{% image examples/putting_interesting_2 %}

After zooming in, more detail begins to emerge.

{% image examples/putting_interesting_3 %}

This detail would have been lost by looking only at the first graph.

{% image examples/putting_interesting_4 %}

