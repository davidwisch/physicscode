# File: src/python/basic_loops_map1.py

# Adds 3 to every element in a list

# Define a function
def add3(val):
	return val + 3

values = map(add3, [1,2,3,4,5])

print values
