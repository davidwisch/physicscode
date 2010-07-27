# File: src/python/basic_arrays_looping3.py

# Demonstrates how to loop through a 2D NumPy array

import numpy

arr2D = numpy.array([
	[1,2,3,4,5],
	[6,7,8,9,10],
	[11,12,13,14,15]
])

# Not very usefull
for line in arr2D:
	for num in line:
		print num
