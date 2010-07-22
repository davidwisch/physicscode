! File: src/fortran/basic_mathematical_operations.f95

PROGRAM basic_mathematical_operations
	! Declare some variables
	CHARACTER(LEN=20) :: str1, str2
	INTEGER :: int1, int2
	REAL :: float1, float2, PI
	
	! Declare some more variables
	INTEGER :: add1, sub1, sub2, mul1, pow1
	CHARACTER(LEN=100) :: con1, con2
	REAL :: add2, add3, add4, sub3, sub4, mul2, mul3, div1, div1f1, & 
		div1f2, div2, pow2, sin1, cos1, tan1, ex, log1, log2, log3
	
	! Initialize some variables
	PI = 4.0 * ATAN(1.0)
	str1 = "I am string #1"
	str2 = "I am string #2"
	int1 = 12
	int2 = 58
	float1 = 100.2
	float2 = 56.3
	
	! Addition ==============================================
	add1 = int1 + int2
	add2 = float1 + float2
	add3 = int1 + int2 + float1 + float2
	add4 = add1 + add2
	
	PRINT *, "\nADDITION:"
	PRINT *, int1, " + ", int2, " = ", add1
	PRINT *, float1, " + ", float2, " = ", add2
	PRINT *, "All numbers added together: ", add3
	PRINT *, "Same Result: ", add4
	
	! Subtraction ===========================================
	sub1 = int1 - int2
	sub2 = int2 - int1
	sub3 = float1 - float2
	sub4 = int1 - int2 - float1 - float2
	
	PRINT *, "\nSUBTRACTION:"
	PRINT *, int1, " - ", int2, " = ", sub1
	PRINT *, int2, " - ", int1, " = ", sub2
	PRINT *, float1, " - ", float2, " = ", sub3
	PRINT *, "All subtracted: ", sub4
	
	! Multiplication ========================================
	mul1 = int1 * int2
	mul2 = float1 * float2
	mul3 = int1 * int2 * float1 * float2
	
	PRINT *, "\nMULTIPLICATION:"
	PRINT *, int1, " x ", int2, " = ", mul1
	PRINT *, float1, " x ", float2, " = ", mul2
	PRINT *, "All multiplied: ", mul3
	
	! Division ==============================================
	div1 = int1 / int2 !gotcha!
	div1f1 = REAL(int1) / int2
	div1f2 = 1.0 * int1 / int2
	div2 = float1 / float2
	
	! Note on the gotcha:
	! A math operations between two integers will never produce a float unless
	! you cast one of the integers as a real before the operation.  This can
	! be done either by multiplying by 1.0 or by using the REAL() function to
	! cast as a float.
	
	PRINT *, "\nDIVISION:"
	PRINT *, int1, " / ", int2, " = ", div1, " - watch out for this!"
	PRINT *, int1, " / ", int2, " = ", div1f1, " - Correct!"
	PRINT *, int1, " / ", int2, " = ", div1f2, " - Correct!"
	PRINT *, float1, " / ", float2, " = ", div2, " - Both were floats, correct!"
	
	! Powers ================================================
	pow1 = 4 ** 6
	pow2 = 3.2 ** 4.7
	
	PRINT *, "\nPOWERS:"
	PRINT *, "4^6", " = ", pow1
	PRINT *, "3.2^4.7", " = ", pow2
	
	! Sin, Cos, Tan =========================================
	sin1 = SIN(PI)
	cos1 = COS(PI)
	tan1 = TAN(PI)
	
	PRINT *, "\nSin, Cos, Tan"
	PRINT *, "Sin(pi) = ", sin1, " - basically zero"
	PRINT *, "Cos(pi) = ", cos1
	PRINT *, "Tan(pi) = ", tan1, " - basically zero"
	
	! Logs ==================================================
	! The following functions' arguments cannot be integers
	ex = EXP(4.0)
	log1 = LOG(4.0)
	log2 = LOG10(4.0)
	
	PRINT *, "\nLogarithms"
	PRINT *, "e^4 = ", ex
	PRINT *, "log_e(4) = ", log1
	PRINT *, "log_10(4) = ", log2
	
	! String Concatenation ==================================
	con1 = TRIM(str1) // TRIM(str2)
	
	PRINT *, "\nSTRING CONCATENATION"
	PRINT *, TRIM(str1), " + ", TRIM(str2), " = ", TRIM(con1)
END PROGRAM basic_mathematical_operations
