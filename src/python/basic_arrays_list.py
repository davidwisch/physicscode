# File: src/python/basic_arrays_list.py

# Demonstrates use of the Python list data type

# Declare an empty list
l = []
# Declare a list with initial values
l2 = ["initial", "values", "are", "cool"]

# Add items to the empty list
l.append("Bob")
l.append("John")
l.append("Mary")

# Print the lists
print l
print l2

# Access an item in the list
print "The first item in 'l' is:", l[0]

# Remove items from the list
l.remove("John") 
l2.remove("initial")
