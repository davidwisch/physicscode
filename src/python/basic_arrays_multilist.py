# File: src/basic_arrays_multilist.py

# Demonstrates how to create a multidimentional array using Python lists

# Declare a list with 4 lists as its elements
list2D = [
	[00,01,02,03],
	[10,11,12,13],
	[20,21,22,23],
	[33,31,32,33]
]

# Access an element from the list (row, then column)
item = list2D[1][1] #index (1,2), Python counts from 0
print "Item at (2,2):", item

# Change a value in the list
list2D[1][1] = 14

# Make sure it worked
item = list2D[1][1]
print "Item at (2,2) is now:", item
