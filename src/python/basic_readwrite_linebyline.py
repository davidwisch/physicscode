# File: src/python/basic_readwrite_linebyline.py

# This program ingests an entire file into an array with each element
# representing a single line from the datafile.

f = open("sampledata.txt", "r") #when reading a file, the 2nd arg is optional

# read all the data into 'datafile'
lines = f.readlines()

#close the connection to our file
f.close()

# start post-processing

#we also want the columns as arrays
for i in range(len(lines)):
	lines[i] = lines[i].split(",") #our columns were separated with commas
	#lines[i] = lines[i].split(" ") #if our columns were separated with spaces

#we can now access our data more easily
print  lines[3][2] #line 4, col, 3 - python counts from 0

#However, to use mathematically, we'll have to cast as a number (values are a string right now)
print int(lines[3][2]) + float(lines[3][2])
