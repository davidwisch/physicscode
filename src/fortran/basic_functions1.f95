! File: src/fortran/basic_functions1.f95

! Basic example of an embedded function that sums two numbers together

PROGRAM basic_functions1
	IMPLICIT NONE

	REAL :: total

	total = add_nums(5.0, 8.0)

	PRINT *, "Sum is: ", total

CONTAINS
	REAL FUNCTION add_nums(num1, num2)
		!IMPLICIT NONE is inherited
		REAL :: num1, num2

		add_nums = num1 + num2
		RETURN !optional in this context
	END FUNCTION add_nums
END PROGRAM basic_functions1
