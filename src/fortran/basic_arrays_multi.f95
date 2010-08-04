! File: src/fortran/basic_arrays_multi.f95

! Demonstrates the use of a multidimentional array in Fortran

PROGRAM basic_arrays_multi
	IMPLICIT NONE

	! Declare a 4x4 array
	REAL, DIMENSION(4,4) :: arr2D = 0.0 ! default
	INTEGER i, j

	! Populate with some dummy values while outputting the array
	DO i=1, 4
		DO j=1, 4
			arr2D(i, j) = i * j ! write the value to the array
			WRITE(*, 10, ADVANCE="NO") arr2D(i, j), " " ! output
		END DO
		PRINT *, ""
	END DO

	10 FORMAT(f4.1, A)
END PROGRAM basic_arrays_multi
