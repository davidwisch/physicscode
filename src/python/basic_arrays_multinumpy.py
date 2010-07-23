# File: src/python/basic_arrays_multinumpy.py

# Demonstrates the use of the NumPy array library to make multidimentional arrays

# Import the NumPy library
import numpy

# Declare a 2D array
arr2D = numpy.array([
	[00,01,02,03],
	[10,11,12,13],
	[20,21,22,23],
	[30,31,32,33]
	])

# Access an element
item = arr2D[1][1]
print "Item (2,2):", item

# Change an items value
arr2D[1][1] = 14
item = arr2D[1][1]
print "Item (2,2) is now:", item
