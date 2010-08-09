# File: src/python/physics_sampling.py

# Samples cos(x^2) 1000 times storing the data in an array then printing it out

import math

# define the function
def cosXSquared(x):
	return math.cos(x**2)

# define some variables
values = []
samples = 1000
limit = math.pi
dx = limit / samples

# sample the function
for i in range(samples):
	val = cosXSquared( i * dx )
	values.append([i * dx, val])

# output the results
for val in values:
	print "Value at:", val[0], "is:", val[1]
