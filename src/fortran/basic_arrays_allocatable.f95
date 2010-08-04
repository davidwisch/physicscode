! File: src/fortran/basic_arrays_allocatable.f95

! Demonstrates the use of allocatable arrays in Fortran

PROGRAM basic_arrays_allocatable
	IMPLICIT NONE

	! Declare an allocatable 1D array
	REAL, ALLOCATABLE, DIMENSION(:) :: arr1D
	! Declare an allocatable 2D array
	REAL, ALLOCATABLE, DIMENSION(:,:) :: arr2D

	! Allocate our 1D array a length of 4
	ALLOCATE(arr1D(4))
	! Allocate our 2D array a dimension of 4x4
	ALLOCATE(arr2D(4, 4))
END PROGRAM basic_arrays_allocatable
