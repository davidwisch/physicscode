# File: src/python/basic_arrays_numpy.py

# Demonstrates the use of NumPy arrays

# Import the NumPy library
import numpy

# Create a 1D array of float64 values
arr = numpy.array([1.0,2.0,3.0,4.0])

# Output the array
print arr

# Add an element to the end of 'arr'
numpy.append(arr, 5.0)

# Access an element
item = arr[0] #Python counts from 0
print "Element 1: ", item
