! File: src/fortran/basic_functions_subroutine.f95

! Demonstrates how to use a subroutine to calculate sample standard deviation

PROGRAM basic_functions_subroutine
	IMPLICIT NONE

	INTEGER :: nums(8)
	REAL :: sd

	nums = (/ 25, 35, 10, 17, 29, 14, 21, 31 /)

	CALL findSD(nums, sd)

	PRINT *, "Standard Deviation: ", sd
END PROGRAM basic_functions_subroutine

SUBROUTINE findSD(nums, sd)
	IMPLICIT NONE

	INTEGER, INTENT(IN) :: nums(8) ! can only be read
	REAL, INTENT(OUT) :: sd ! will be written, undefined
	INTEGER :: i
	REAL :: inner_sum = 0.0, mean

	mean = SUM(nums) / FLOAT(SIZE(nums))

	DO i = 1, SIZE(nums)
		inner_sum = inner_sum + (nums(i) - mean)**2.0
	END DO
	inner_sum = inner_sum * (1.0 / (SIZE(nums) - 1))
	sd = SQRT(inner_sum) ! set the value of 'sd'
END SUBROUTINE
