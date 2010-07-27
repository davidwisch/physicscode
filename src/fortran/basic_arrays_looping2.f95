! File: src/fortran/basic_arrays_looping2.f95

! Demonstrates method for looping through 2D data

PROGRAM basic_arrays_looping2
	IMPLICIT NONE

	INTEGER :: i, j
	INTEGER, DIMENSION(4,2) :: arr2D

	! Fill arr2D with data
	DO i = 1, 4
		DO j = 1, 2
			arr2D(i, j) = i * j
		END DO
	END DO

	! Output arr2D contents
	DO i = 1, 4
		DO j = 1, 2
			PRINT *, "Index (", i,", ", j, ") is: ", arr2D(i, j)
		END DO
	END DO
END PROGRAM basic_arrays_looping2
