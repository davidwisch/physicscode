# File: src/python/basic_control_andor.py

# Demonstrates the use of "and" and "or" conditionals.

name = raw_input("What is your name? ")

if name == "John" or name == "Steve":
	print "Your name is John or Steve"
elif name != "Jenny" and len(name) <= 6:
	print "Your name has 6 or fewer letters and is not Jenny"
else:
	print "Your name doesn't fit into our random conditions"
