# File: src/python/basic_loops_map2.py

# Convert a lists values to floating point numbers

lines = [
	["1", "2", "3"],
	["4", "5", "6"],
	["7", "8", "9"]
]

for i in range(len(lines)):
	lines[i] = map(float, lines[i])
