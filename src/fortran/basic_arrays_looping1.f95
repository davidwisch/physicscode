! File: src/fortran/basic_arrays_looping1.f95

! Demonstrates method for looping through 1D data

PROGRAM basic_arrays_looping1
	IMPLICIT NONE

	INTEGER :: i, vals(6)

	vals= (/ 1, 2, 3, 4, 5, 6/)

	DO i = 1, SIZE(vals)
		PRINT*, vals(i)
	END DO
END PROGRAM
