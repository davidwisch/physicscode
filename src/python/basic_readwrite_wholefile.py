# File: src/python/basic_readwrite_wholefile.py

# This program ingests an entire file at once into a single variable

f = open("sampledata.txt", "r") #when reading a file, the 2nd arg is optional

# read all the data into 'datafile'
datafile = f.read()

#close the connection to our file
f.close()

# start post-processing

# we'll want to look at the data line-by-line, so we'll need to split it
#split based on newlines, ignore the last line (we have a trailing newline)
lines = datafile.split("\n")[:-1]

#we also want the columns as arrays
for i in range(len(lines)):
	lines[i] = lines[i].split(",") #our columns were separated with commas
	#lines[i] = lines[i].split(" ") #if our columns were separated with spaces

#we can now access our data more easily
print  lines[3][2] #line 4, col, 3 - python counts from 0

#However, to use mathematically, we'll have to cast as a number (values are a string right now)
print int(lines[3][2]) + float(lines[3][2])
