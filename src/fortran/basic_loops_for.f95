! File: src/fortran/basic_loops_for.f95

! Demonstrates the use of the DO loop in Fortran

PROGRAM basic_loops_for
	IMPLICIT NONE

	INTEGER :: i

	DO i = 0, 24, 1 ! could ommit the 3rd parameter
		PRINT *, "Index is: ", i
	END DO
END PROGRAM basic_loops_for
