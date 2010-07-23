! File: src/fortran/basic_arrays_1dshort.f95

! Demonstrates the use of a Fortran array constructor

PROGRAM basic_arrays_1dshort
	IMPLICIT NONE

	REAL :: arr1D(3)
	INTEGER :: i

	! Short syntax for declaring an array
	arr1D = (/ 1.0, 2.0, 3.0 /)

	! Print out the array
	DO i=1, SIZE(arr1D)
		PRINT *, "Val at ", i, " is: ", arr1D(i)
	END DO
END PROGRAM basic_arrays_1dshort
