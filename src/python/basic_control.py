# File: src/python/basic_control.py

# Demonstrates the use of if/elif/else statements.

# The following line is unrelated to control statements
num = int(raw_input("Input Number:"))

if num > 0:
	print num, "is positive"
elif num < 0:
	print num, "is negative"
else:
	print num, "is zero"
