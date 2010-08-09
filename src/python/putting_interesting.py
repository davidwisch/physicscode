# File: src/python/putting_interesting.py

# Sample a function and write out to a file

import math

min = 1.0
max = 5.0
points = 250000
dx = (max - min) / points
index = min

# Define the function
def fun(x):
	top = 25.0 * math.cos(3.0 * x) * math.sin(2.0 * x) * math.log(x**2.0)
	bottom = math.log( abs(x**2.0 - 10.0) )

	return top / bottom

# Sample and write out
outfile = open("output.txt", "w")
while index <= max:
	outfile.write(
		str(index) + " " +
		str(fun(index)) + "\n"
		)
	index += dx
outfile.close()
