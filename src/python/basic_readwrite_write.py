# File: src/python/basic_readwrite_write.py

# This program writes sevral lines out to a text file

# Open a connection to our file
f = open("outfile.txt", "w") # here, the 2nd arg is NOT optional

f.write("This will be line #1\n")
f.write("This will be line #2\n")
f.write("This will be line #3\n")

# close our file connection
# In Python, data is not written to the file until one of the close()
# or flush() methods is called
f.close()
