! File: src/fortran/basic_functions3.f95

! A function that calculates the sample standard deviation of a set of numbers

PROGRAM basic_functions3
	IMPLICIT NONE

	INTEGER :: nums(8)
	REAL :: sd

	nums = (/ 25, 35, 10, 17, 29, 14, 21, 31 /)

	sd = findSD(nums)
	PRINT *, "Standard Deviation: ", sd

CONTAINS
	REAL FUNCTION findSD(nums)
		! IMPLICIT NONE is inherited
		INTEGER :: nums(8), i
		REAL :: inner_sum = 0.0, mean

		mean = SUM(nums) / FLOAT(SIZE(nums))

		DO i = 1, SIZE(nums)
			inner_sum = inner_sum + (nums(i) - mean)**2.0
		END DO
		inner_sum = inner_sum * (1.0 / (SIZE(nums) - 1))
		findSD = SQRT(inner_sum)

		RETURN
	END FUNCTION findSD
END PROGRAM basic_functions3
