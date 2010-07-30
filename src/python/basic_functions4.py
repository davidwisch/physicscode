# File: src/python/basic_functions4.py

# A function that calculates the sample standard deviation of a set of numbers

import math

# Define our function
def find_sd(nums):
	mean = sum(nums) / float(len(nums)) #find the average, float() to ensure precision
	inner_sum = 0
	for num in nums: # find then inner sum
		inner_sum += (num - mean) ** 2
	inner_sum *= (1.0 / (len(nums) - 1)) # apply the 1/N-1 factor
	return math.sqrt(inner_sum)

# Now let's call the function and print the sum
numbers = [25, 35, 10, 17, 29, 14, 21, 31]
sd = find_sd(numbers)
print "Standard Deviation:", sd
