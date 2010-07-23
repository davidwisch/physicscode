# File: src/python/basic_arrays_array.py

# Demonstrates the use of the Python array class

# Import the array class
import array

# Declare an array of doubles with initial values
arr = array.array('d', [1.2, 2.4, 5.7, 8.9])
# Declare an empty array of type double
arr2 = array.array('d')

# Add values to both arrays
arr.append(4.5)
arr2.append(4.5)

# Print both arrays
print arr
print arr2

# Access an array element
print "The first item in arr is:", arr[0]

# Remove an item from 'arr'
arr.remove(1.2)
