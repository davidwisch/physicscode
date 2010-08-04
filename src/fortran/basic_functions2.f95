! File: src/fortran/basic_functions2.f95

! Basic example of an external function that sums two numbers together

PROGRAM basic_functions2
	IMPLICIT NONE

	REAL :: total, add_nums ! declare our function

	total = add_nums(5.0, 8.0)

	PRINT *, "Sum is: ", total
END PROGRAM basic_functions2

! External function
REAL FUNCTION add_nums(num1, num2)
	IMPLICIT NONE !not inherited
	REAL :: num1, num2

	add_nums = num1 + num2
	RETURN !optional in this context
END FUNCTION add_nums
