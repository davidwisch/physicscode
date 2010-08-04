! File: src/fortran/basic_arrays_1d.f95

! Demonstrates a 1D array in Fortran

PROGRAM basic_arrays_1d
	IMPLICIT NONE

	! Declare a array of length 5 of type REAL
	REAL, DIMENSION(5) :: arr1D = 0.0 ! default
	INTEGER :: i

	! Fill the array with values
	arr1D(1) = 4.0 ! Fortran counts from 1
	arr1D(2) = 5.0
	arr1D(3) = 6.0
	arr1d(4) = 7.0
	arr1d(5) = 8.0

	! Print out the array
	DO i=1, SIZE(arr1D)
		PRINT *, "Val at ", i, " is: ", arr1D(i)
	END DO
END PROGRAM basic_arrays_1d
