# File: src/python/basic_mathematical_operations.py

# First, let's import the 'math' library
import math

# Declare some variables so we have data to play around with
str1 = "I am string #1"
str2 = "I am string #2"
int1 = 12
int2 = 58
float1 = 100.2
float2 = 56.3

# Addition ==================================================
add1 = int1 + int2
add2 = float1 + float2
add3 = int1 + int2 + float1 + float2
add4 = add1 + add2

print "ADDITION:"
print int1, "+", int2, "=", add1
print float1, "+", float2, "=", add2
print "All numbers added together:", add3
print "Same Result:", add4

# Subtraction ===============================================
sub1 = int1 - int2
sub2 = int2 - int1
sub3 = float1 - float2
sub4 = int1 - int2 - float1 - float2

print "\nSUBTRACTION:"
print int1, "-", int2, "=", sub1
print int2, "-", int1, "=", sub2
print float1, "-", float2, "=", sub3
print "All subtracted:", sub4

# Multiplication ============================================
mul1 = int1 * int2
mul2 = float1 * float2
mul3 = int1 * int2 * float1 * float2

print "\nMULTIPLICATION:"
print int1, "x", int2, "=", mul1
print float1, "x", float2, "=", mul2
print "All multiplied:", mul3

# Division ==================================================
div1 = int1 / int2 #gotcha!
div1f1 = float(int1) / int2
div1f2 = 1.0 * int1 / int2
div2 = float1 / float2

# Note on the gotcha:
# A math operation between two integers will never produce a float unless
# one you cast one of the integers to a float before the operation.  This can
# be done either by multiplying by 1.0 or using the float() function to cast
# as a float.

print "\nDIVISION:"
print int1, "/", int2, "=", div1, "- Watch out for this!"
print float(int1), "/", int2, "=", div1f1, "- Correct!"
print 1.0 * int1, "/", int2, "=", div1f2, "- Correct!"
print float1, "/", float2, "=", div2, "- Both were floats, correct!"

# Powers ====================================================
pow1 = int1 ** int2
pow2 = float1 ** float2

print "\nPOWERS:"
print int1, "^", int2, "=", pow1
print float1, "^", float2, "=", pow2

# Sin, Cos, Tan
# math.pi is pi, stored as a constant in the math library
sin1 = math.sin(math.pi)
cos1 = math.cos(math.pi)
tan1 = math.tan(math.pi)

# there are similar functions for arcsin, arccos, and arctan, see the python
# documentation for more details (http://docs.python.org/library/math.html) 

print "\nSin, Cos, Tan"
print "Sin(pi) =", sin1
print "Cos(pi) =", cos1
print "Tan(pi) =", tan1

# Logs =======================================================
ex = math.exp(4)
log1 = math.log(4)
log2 = math.log(4, 10)
log3 = math.log(4, 3)

print "\nLogarithms:"
print "e^4 =", ex
print "log_e(4) =", log1
print "log_10(4) =", log2
print "log_3(4) = ", log3

# String Concatination =======================================
con1 = str1 + str2
con2 = str1 * 4

print "\nSTRING CONCATINATION"
print str1, "+", str2, "=", con1
print str1, "x 4 =", con2
