# File: src/python/basic_arrays_looping4.py

# Demonstrates 'traditional' method for looping through 2D data

import numpy

arr2D = numpy.array([
	[1,2,3,4,5],
	[6,7,8,9,10],
	[11,12,13,14,15]
])

for i in range(len(arr2D)):
	for j in range(len(arr2D[i])):
		print "Index (",i,",",j,") is:", arr2D[i][j]
